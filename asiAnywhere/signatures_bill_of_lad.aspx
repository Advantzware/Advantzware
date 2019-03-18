<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="signatures_bill_of_ipad" Codebehind="signatures_bill_of_lad.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<%@ Register TagPrefix="uc" TagName="Signature"  Src="~/SignatureControl/ctlSignature.ascx" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Capture Signature On Bill of Lading Report</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language = JavaScript>
    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
     
 
    
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].TextBox1.value = ReturnObj1;
  document.forms[0].TextBox1.focus();
  
    
}

function bolnumlook() {
    var cust = document.forms[0].TextBox1.value;
    var NewWindow = window.open("bolnum_rep_lookup.aspx?customer=" + cust + "&post="+ "Yes" +"", "BolNumLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function bolnumreplook(ReturnObj1, ReturnObj2){
    document.forms[0].TextBox3.value = ReturnObj1;
    document.forms[0].BolFmtHiddenField.value = ReturnObj2;
    document.getElementById("TextBox3").onchange();
    document.forms[0].TextBox3.focus();
}

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].TextBox9.value=obj;
}
function Datelook1()
{
  document.forms[0].TextBox9.value="";
  Datelook();
}
    </script> 
    <script language="javascript">
        function EnableSaveButton(enable) {
           
            var btnSave = document.getElementById("submitbutton");

            if (enable) {
                if (window.frmSign.IsValid) {
                    btnSave.disabled = "";
                }
                else {
                    btnSave.disabled = "disabled";
                }
            }
            else
                btnSave.disabled = "disabled";
        }
		  
		</script>

      
  </head>    
   <body>
    <form id="frmList" runat="server" defaultfocus="TextBox1" >   
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
        
        <div id="div1" style="display:block">                      
        <embed src="<%= "Signatures/" + fname %>" width="1000" height="420" /> 
        <br />
        <fieldset style="width:180">
         <table class="shade">     
         <%--<tr><td align="left" style="padding-right: 5px" valign="top"><b>Please Sign Below:</b></td></tr>
         <tr>   
         <td align="left"><table><tr>                                     
         <td align="right" nowrap><b>Signature-> </b> <br /> <br /> <br /> <br /> <b> Date-> </b></td>
         <td><uc:Signature id="ctlMySignature" PenColor="Black" PenWidth="2" BackColor="White" SignWidth="500" SignHeight="125"
            SavePath="~/Signatures/" SignatureCodePath="~/SignatureControl/" SignatureFileFormat="jpeg" Runat="server"></uc:Signature>
         </td>    
         <td align="left" nowrap><b> <-Carrier </b> <br /> <br /> <br /> <br /> <b> <-Date</b></td>     
         </tr></table></td> 
         </tr>--%>
         
         <tr><td align="left" style="padding-right: 5px" valign="top"><b>Please Sign Below:</b></td></tr>
         <tr>   
         <td align="left"><table><tr>                                     
         <td align="right" nowrap><b><asp:Label ID="Label3" runat="server" Text="Signature->"></asp:Label> </b> <br /> <br /> <br /> <br /> <b> <asp:Label ID="Label4" runat="server" Text="Date->"></asp:Label> </b></td>
         <td><uc:Signature id="ctlMySignature" PenColor="Black" PenWidth="2" BackColor="White" SignWidth="500" SignHeight="125"
            SavePath="~/Signatures/" SignatureCodePath="~/SignatureControl/" SignatureFileFormat="jpeg" Runat="server"></uc:Signature>
         </td>    
         <td align="left" nowrap><b> <asp:Label ID="Label5" runat="server" Text="<-Carrier"></asp:Label> </b> <br /> <br /> <br /> <br /> <b> <asp:Label ID="Label6" runat="server" Text="<-Date"></asp:Label></b></td>     
         </tr></table></td> 
         </tr>
                  
                       
         <tr>                                    
         <td align="center">
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         <input type="button" class="button" value="  Save  "  onclick="SaveSignature(); EnableSaveButton(true);"><input type="button" class="button"  value="  Re-Set " onclick="ClearSignature(); EnableSaveButton(false);">
         <asp:Button ID="submitbutton" OnClick="submitbutton_click" disabled="disabled"  runat="server"  Text="PrintSignedBOL" />
         </td><td>&nbsp;</td></tr>                             
         </table>  
         </fieldset>
         </div>      
         
         <% } %>
         
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
    
    <script language="javascript">
//        if (document.forms[0].TextBox3.value != "") {
//            document.getElementById("div1").style.display = "block";
//        }
//        else {
//            document.getElementById("div1").style.display = "none";
//        }
    </script>
    
    </form>
  </body>
</HTML>


