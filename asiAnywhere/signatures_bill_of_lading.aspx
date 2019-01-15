<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="signatures_bill_of_lading" Codebehind="signatures_bill_of_lading.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <meta http-equiv="X-UA-Compatible" content="chrome=1,IE=edge" />
    <title>Capture Signature On Bill of Lading Report</title>
    <style type="text/css"><!--
      #container { position: relative;  }
      #imageView { border: dashed ; border-width:1px; cursor:pointer; }
    --></style>
    <script src="include/jquery-1.4.2.min.js"  type="text/javascript"></script>
    <script type="text/javascript" src="include/example2.js"></script>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
  
    <script language="javascript" type="text/javascript">

        function contactcustomerlook() {
            var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
            document.forms[0].TextBox1.value = ReturnObj1;
            document.forms[0].TextBox1.focus();


        }

        function bolnumlook() {
            var cust = document.forms[0].TextBox1.value;
            var NewWindow = window.open("bolnum_rep_lookup.aspx?customer=" + cust + "&post=" + "Yes" + "", "BolNumLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function bolnumreplook(ReturnObj1, ReturnObj2) {
            document.forms[0].TextBox3.value = ReturnObj1;
            document.forms[0].BolFmtHiddenField.value = ReturnObj2;
            document.getElementById("TextBox3").onchange();
            document.forms[0].TextBox3.focus();
        }

        function EnableSaveButton(enable) {

            var btnSave = document.getElementById("submitbutton");

            if (enable) {
                btnSave.disabled = "";
            }
            else
                btnSave.disabled = "disabled";
        }
        
		  
		</script>
		<script type="text/javascript">
		    window.onload = oncolorwhite;

		    function oncolorwhite() {
		        var canvas2 = document.getElementById("imageView");
		        var context2 = canvas2.getContext("2d");

		        context2.fillStyle = "White";
		        context2.fillRect(0, 0, 500, 125);
		    }

		    function wireButtonEvents() {

		        var btnClear = document.getElementById("btnClear");
		        var canvas = document.getElementById("imageView");
		        var context = canvas.getContext("2d");

		        context.clearRect(0, 0, 500, 125);
		        context.fillStyle = "White";
		        context.fillRect(0, 0, 500, 125);
		        var divsign = document.getElementById("getcan");
		        divsign.style.display = "inline";
		        var divim = document.getElementById("getimage");
		        divim.style.display = "none";

		    }

		    // Send the canvas image to the server.

		    $(function() {

		        $("#btnSave").click(function() {

		            var image = document.getElementById("imageView").toDataURL("image/png");

		            image = image.replace('data:image/png;base64,', '');


		            $.ajax({

		                type: 'POST',

		                url: 'CanvasSave.aspx/UploadImage',

		                data: '{ "imageData" : "' + image + '" }',

		                contentType: 'application/json; charset=utf-8',

		                dataType: 'json',

		                success: function(msg) {

		                    alert('Signature saved successfully, ready to PrintSignedBol');
		                    var divim = document.getElementById("getimage");
		                    divim.style.display = "inline";
		                    var canvas = document.getElementById("imageView"); // save canvas image as data url (png format by default)
		                    var dataURL = canvas.toDataURL("image/png");
		                    document.getElementById("saveSignuater").src = dataURL;
		                    var divsign = document.getElementById("getcan");
		                    divsign.style.display = "none";

		                }

		            });

		        });

		    });

</script>

<script type="text/javascript" >
    var navigatorVersion = navigator.appVersion;
    var navigatorAgent = navigator.userAgent;
    var browserName = navigator.appName;
    var fullVersionName = '' + parseFloat(navigator.appVersion);
    var majorVersionName = parseInt(navigator.appVersion, 10);
    var nameOffset, verOffset, ix;

    // In Firefox, the true version is after "Firefox"
    if ((verOffset = navigatorAgent.indexOf("Firefox")) != -1) {
        browserName = "Firefox";
        fullVersionName = navigatorAgent.substring(verOffset + 8);
    }
    // In MSIE, the true version is after "MSIE" in userAgent
    else if ((verOffset = navigatorAgent.indexOf("MSIE")) != -1) {
        browserName = "MSIE";
        fullVersionName = navigatorAgent.substring(verOffset + 5);
    }

    // In Chrome, the true version is after "Chrome"
    else if ((verOffset = navigatorAgent.indexOf("Chrome")) != -1) {
        browserName = "Chrome";
        fullVersionName = navigatorAgent.substring(verOffset + 7);
    }

    // In Opera, the true version is after "Opera" or after "Version"
    else if ((verOffset = navigatorAgent.indexOf("Opera")) != -1) {
        browserName = "Opera";
        fullVersionName = navigatorAgent.substring(verOffset + 6);
        if ((verOffset = navigatorAgent.indexOf("Version")) != -1)
            fullVersionName = navigatorAgent.substring(verOffset + 8);
    }

    // In Safari, the true version is after "Safari" or after "Version"
    else if ((verOffset = navigatorAgent.indexOf("Safari")) != -1) {
        browserName = "Safari";
        fullVersionName = navigatorAgent.substring(verOffset + 7);
        if ((verOffset = navigatorAgent.indexOf("Version")) != -1)
            fullVersionName = navigatorAgent.substring(verOffset + 8);
    }

    // In most other browsers, "name/version" is at the end of userAgent
    else if ((nameOffset = navigatorAgent.lastIndexOf(' ') + 1) <
          (verOffset = navigatorAgent.lastIndexOf('/'))) {
        browserName = navigatorAgent.substring(nameOffset, verOffset);
        fullVersionName = navigatorAgent.substring(verOffset + 1);
        if (browserName.toLowerCase() == browserName.toUpperCase()) {
            browserName = navigator.appName;
        }
    }
    // trim the fullVersionName string at semicolon/space if present
    if ((ix = fullVersionName.indexOf(";")) != -1)
        fullVersionName = fullVersionName.substring(0, ix);
    if ((ix = fullVersionName.indexOf(" ")) != -1)
        fullVersionName = fullVersionName.substring(0, ix);

    majorVersionName = parseInt('' + fullVersionName, 10);
    if (isNaN(majorVersionName)) {
        fullVersionName = '' + parseFloat(navigator.appVersion);
        majorVersionName = parseInt(navigator.appVersion, 10);
    }



    if (browserName == "MSIE" && fullVersionName == "7.0") {

        window.location.href = ("signatures_bill_of_lad.aspx");
    }

    if (browserName == "MSIE" && fullVersionName == "8.0") {

        window.location.href = ("signatures_bill_of_lad.aspx");
    }
    if (browserName == "MSIE" && fullVersionName == "9.0") {

        window.location.href = ("signatures_bill_of_lad.aspx");
    }

   
   </script>

      
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus="TextBox1" >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>       
            
                       
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=left nowrap><font size=+0><b>Capture Signature On Bill of Lading&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <asp:HiddenField ID="HiddenField1" runat="server" />
         <asp:HiddenField ID="HiddenField2" runat="server" />
         <asp:HiddenField ID="HiddenField3" runat="server" />
         <asp:HiddenField ID="HiddenField4" runat="server" />
         
         <asp:HiddenField ID="HiddenField7" runat="server" />
         <asp:HiddenField ID="HiddenField8"  runat="server" />
         <asp:HiddenField ID="BolFmtHiddenField"  runat="server" />
         
          <asp:Label ID="Label1" Font-Bold="true" ForeColor="red" runat="server" ></asp:Label>
          
                    
      <table class="shade" >
     
      <tr><td align="right" style="padding-right: 5px"><b>Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1" MaxLength="8" width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b> BOL#:</b></td>
          <td nowrap><asp:TextBox  ID="TextBox3" runat="server" MaxLength="8" AutoPostBack="true" CausesValidation="true" OnTextChanged="bol_textbox_change"   Width="100px" ></asp:TextBox>          
          <a href="#" tabindex="1" onClick="bolnumlook(); return false"><asp:Image ID="BolLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>             
            &nbsp;&nbsp;&nbsp;<asp:Label ID="Label2" runat="server" ForeColor="Red"></asp:Label>
              
          </td><td>
         
          </td>
        </tr>
        </table> 
        <br />
        
         
         <%  
            string fname = TextBox3.Text.Trim() + "PrintedBOL.pdf";
            string filepath = Server.MapPath("Signatures") + "\\" + fname;                                           
            if (System.IO.File.Exists(filepath))
            {               
        %>
        
        <div id="div1" style="display:inline">                      
        <embed src="<%= "Signatures/" + fname %>" width="1000" height="420" /> 
      
        <br />
        <% } %>
        <br />
         </div>
         
      
         
         <div>
        <fieldset id="mainfld" runat="server" class="shade"  style="width:180;display:none;">
         <table >         
         
         <tr><td align="left" style="padding-right: 5px" valign="top"><b>Please Sign Below:</b></td></tr>
         <tr>   
         <td align="left"><table><tr>                                     
         <td align="right" nowrap><b><asp:Label ID="Label3" runat="server" Text="Signature->"></asp:Label> </b> <br /> <br /> <br /> <br /> <b> <asp:Label ID="Label4" runat="server" Text="Date->"></asp:Label> </b></td>
         <td>         
      <div id="container">
      <div id="getcan" >
      <canvas id="imageView" width="500" height="125">
        <p>Unfortunately, your browser is currently unsupported by our web 
        application.  We are sorry for the inconvenience. Please use one of the 
        supported browsers listed below.</p>
        <p>Supported browsers: <a href="http://www.opera.com">Opera</a>, <a 
          href="http://www.mozilla.com">Firefox</a>, <a 
          href="http://www.apple.com/safari">Safari</a>, and <a 
          href="http://www.konqueror.org">Konqueror</a>.</p>
      </canvas></div>
       <div id="getimage" style="display:none">
      <img id="saveSignuater" alt="Save Image jpg" style="border: dashed ; border-width:1px;" />
      </div> 
      
      <%--<br />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            
      <br />--%>
    </div>
                   
            
         </td>    
         <td align="left" nowrap><b> <asp:Label ID="Label5" runat="server" Text="<-Carrier"></asp:Label> </b> <br /> <br /> <br /> <br /> <b> <asp:Label ID="Label6" runat="server" Text="<-Date"></asp:Label></b></td>     
         </tr></table></td> 
         </tr>
                  
                       
         <tr>                                    
         <td align="center">
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         
         <input type="button" id="btnSave" name="btnSave" class="button" value="  Save  "  onclick="EnableSaveButton(true);" /><input type="button" id="btnClear" name="btnClear" class="button"  value="  Re-Set " onclick="wireButtonEvents(); EnableSaveButton(false);" />
         <asp:Button ID="submitbutton" OnClick="submitbutton_click" disabled="disabled"  runat="server"  Text="PrintSignedBOL" />
         </td><td>&nbsp;</td></tr>                             
         </table>  
         </fieldset>
          
         </div>     
         
        
         
            <asp:FormView ID="FormView1" Visible="false" runat="server" 
              DataSourceID="ObjectDataSource1" OnPreRender="FormView1_PreRender">
                                            
              <ItemTemplate>
                  sign:
                  <asp:Label ID="signLabel" runat="server" Text='<%# Bind("sign") %>'></asp:Label><br />
                  <asp:Label ID="pdfpathnameLabel" runat="server" Text='<%# Bind("pdfpathname") %>'></asp:Label>
              </ItemTemplate>
                
               
               
          </asp:FormView>
      
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectSignatureBol" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmAction" Type="String"  />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmcustno" Type="String" />
                  
                  <asp:Parameter Name="prmbolno" Type="Int32" />
                  <asp:Parameter Name="prmprinted" Type="String" />
                  
                  <asp:Parameter Name="prmposted" Type="String" />
                  <asp:Parameter Name="prmpostbol" Type="String" />
                  <asp:Parameter Name="prmBegDate" Type="String" />
                  <asp:Parameter Name="prmEndDate" Type="String" />
                  <asp:Parameter Name="prmCarrier" Type="String" />
                  <asp:Parameter Name="imagepath" Type="String" />
                  <asp:Parameter Name="prmBegOrder" Type="Int32" />
                  <asp:Parameter Name="prmEndOrder" Type="Int32" />
                  <asp:Parameter Name="prmPage" Type="String" />
                  <asp:Parameter Name="prmPdfPath" Type="String" />
                                  
                  
              </SelectParameters>
          </asp:ObjectDataSource>
    </div>
        
    <ft:footer id="Footer1" runat="server"></ft:footer>
    
        
    </form>
    

   
  </body>
</HTML>


