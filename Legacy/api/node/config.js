/* config.js contain common config information as below: */
// javaProgramDir : this is the relative path (path from where node server is started) where java programs are stored on Node 
// logsDir : this is the relative path (path from where node server is started) where logs are stored on Node if APPServer is down
// javaProgram: this is the name of the java program which gets called by Node for processing a request
// appServerURL: this is the URL to Progress AppServer. This needs update based on the targeted AppServer broker name
// DLC: this should have \\ to escape backslash. DLC needs to change as per the server configuration 
// authUsername and authPassword: this is to be used while making calls to inbound API enpoints. This needs to change accordingly

const config = {"javaProgramDir":"java", 
				"logsDir":"java\\logs\\",
				"javaProgram":"RequestDispatcher",
				"appServerURL":"AppServer://localhost:5162/Advantzware_API",
				"DLC":"C:\\Progress\\OE116_64\\",
				"authUsername":"user1",
				"authPassword":"user1"				
			   };
			   

			   
module.exports = {
    config
}