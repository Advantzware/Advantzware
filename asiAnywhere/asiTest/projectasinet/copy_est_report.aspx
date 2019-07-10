<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="copy_est_list" Codebehind="copy_est_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Copy Estimates</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" type="text/css" href="include/CalendarControl.css" />
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    
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
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }

    var mval = "";
    var dval = "";
    var eval = "";
    

    function estinfolook(val) {
        eval = val;
        var NewWindow = window.open("est_info_lookup.aspx", "EstimateInformation", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function EstimateInfoLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5) {
        if (eval == "1") {
            document.forms[0].BegEstTextBox.value = ReturnObj1;
            document.getElementById("itmnamLabel").innerText = ReturnObj2;

            document.getElementById("shipnamLabel").innerText = ReturnObj3;
            document.forms[0].BegCustTextBox.value = ReturnObj4;
            document.forms[0].custpartTextBox.value = ReturnObj5;
            document.forms[0].BegEstTextBox.onchange();
        }
        else if (eval == "2") {
            document.forms[0].EndEstTextBox.value = ReturnObj1;
            document.forms[0].EndEstTextBox.focus();
        }
    }

    var comp = "";
    function companylook(value) {
        comp = value;
        var NewWindow = window.open("company_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function CompanyLookup(ReturnObj1,ReturnObj2) {
        if (comp == "1") {
            document.forms[0].fromcompTextBox.value = ReturnObj1;
            document.getElementById("frmcompnamLabel").innerText = ReturnObj2;
        }
        else if (comp == "2") {
        document.forms[0].tocompTextBox.value = ReturnObj1;
        document.getElementById("tocompnamlabel").innerText = ReturnObj2;
        }
    }      

    function contactcustomerlook() {
        var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
        document.forms[0].BegCustTextBox.value = ReturnObj1;
        document.forms[0].BegCustTextBox.focus();

    }
    function contactcustomerlook2() {
        var NewWindow = window.open("contact_customer_copylookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerCopyLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {

        document.forms[0].EndCustTextBox.value = ReturnObj1;
        document.forms[0].EndCustTextBox.focus();
    }

    
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='BegCustTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
        
      <div>
      <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
        <asp:HiddenField ID="HiddenField1" runat="server" />   
        <asp:HiddenField ID="HiddenField2" runat="server" />
        <asp:HiddenField ID="HiddenField3" runat="server" />
        <asp:HiddenField ID="HiddenField4" runat="server" />
        <asp:HiddenField ID="HiddenField5" runat="server" />
        <asp:HiddenField ID="HiddenField6" runat="server" />
        <asp:HiddenField ID="HiddenField7" runat="server" />
        <asp:HiddenField ID="HiddenField8" runat="server" />
        <asp:HiddenField ID="HiddenField9" runat="server" />
        
       
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Copy Estimates&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>            
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       
       <fieldset style="width:620px" class="shade" ><legend>Selection Parameters</legend>
      <asp:UpdatePanel id="gridviewupdatepanel" runat="server">
      <ContentTemplate>
       <div >
                <asp:UpdateProgress ID="UpdateProgress1" runat="server" 
                    AssociatedUpdatePanelID="gridviewupdatepanel"
                    DisplayAfter="100" DynamicLayout="true">                    
                    <ProgressTemplate>                       
                        <asp:Label ID="lblProgress" runat="server" ></asp:Label>               
                    Please wait ...             
                    </ProgressTemplate>                    
                </asp:UpdateProgress>
                </div> 
                <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
      <table class="shade" width="620px">
      <tr><td>       
      <fieldset style="width:590px" class="shade" ><legend><asp:Label ID="copyfrom"  runat="server" Text="Copy From" ForeColor="Blue" ></asp:Label></legend>
        <table>           
      <tr><td align="right" style="padding-right: 5px"><b>From Company:</b></td>
          <td nowrap><asp:TextBox ID="fromcompTextBox" Width="100px" AutoPostBack="true" OnTextChanged="FromComp_TextChange" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onclick="companylook(1); return false"><asp:Image ID="compLookup" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
           </td> 
           <td nowrap><asp:Label ID="frmcompnamLabel" Width="100px" runat="server"></asp:Label>            
           </td>       
      </tr> 
      
      <tr><td align="right" style="padding-right: 5px"><b>From Estimate#:</b></td>
          <td nowrap><asp:TextBox ID="BegEstTextBox" Width="100px" AutoPostBack="true" OnTextChanged="BegEstTextBox_TextChange" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="estinfolook('1'); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
           </td>
           <td nowrap><asp:Label ID="itmnamLabel" Width="100px" runat="server"></asp:Label>           
           </td></tr>
       <tr><td></td><td></td><td nowrap> <asp:Label ID="shipnamLabel" Width="100px" runat="server"></asp:Label>           
           </td></tr>                                 
        <tr></tr> 
        <tr>   <td>&nbsp;</td>                   
            <td align="left" style="padding-right:15px">
                <b><asp:CheckBox ID="CheckBox1" Text="Copy Routing?" runat="server"></asp:CheckBox></b>
            </td> 
            <td>
                <b><asp:CheckBox ID="CheckBox5" Text="Copy Estimate Notes?" runat="server"></asp:CheckBox></b>
            </td>       
        </tr>
        <tr>    <td>&nbsp;</td>                            
            <td align="left" style="padding-right:15px">
                <b><asp:CheckBox ID="CheckBox2" Text="Copy Die#?" runat="server"></asp:CheckBox></b>
            </td> 
            <td>
                <b><asp:CheckBox ID="CheckBox6" Text="Copy Item Name?" runat="server"></asp:CheckBox></b>
            </td>           
        </tr>
        <tr>  <td>&nbsp;</td>                 
            <td align="left" style="padding-right:15px">
                <b><asp:CheckBox ID="CheckBox3" Text="Copy Plate#?" runat="server"></asp:CheckBox></b>
            </td>  
            <td>
                <b><asp:CheckBox ID="CheckBox7" Text="Copy Item Description 1?" runat="server"></asp:CheckBox></b>
            </td>          
        </tr>
        <tr>   <td>&nbsp;</td>                  
            <td align="left" style="padding-right:15px">
                <b><asp:CheckBox ID="CheckBox4" Text="Copy FG item#?" runat="server"></asp:CheckBox></b>
            </td>
            <td>
                <b><asp:CheckBox ID="CheckBox8" Text="Copy Item Description 2?" runat="server"></asp:CheckBox></b>
            </td>             
        </tr> 
        <tr>  <td>&nbsp;</td>                  
            <td align="left" style="padding-right:15px">
                <b><asp:CheckBox ID="CheckBox9" Text="Copy Farm Out Costs?" runat="server"></asp:CheckBox></b>
            </td>
            </tr> </table>  </fieldset>
      </td></tr>
       <tr><td>
       <fieldset style="width:590px" class="shade" ><legend><asp:Label ID="Label2" runat="server" Text="Copy To" ForeColor="Blue"></asp:Label></legend>
       <table>
       <tr><td align="right" style="padding-right: 5px"><b>To Company:</b></td>
          <td nowrap><asp:TextBox ID="tocompTextBox" Width="100px" AutoPostBack="true" OnTextChanged="toComp_TextChange" runat="server"></asp:TextBox> 
          <a href="#" tabindex="1" onclick="companylook(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>           
           </td>  
           <td nowrap><asp:Label ID="tocompnamlabel" Width="100px" runat="server"></asp:Label>            
           </td>      
      </tr>    
      <tr><td align="right" style="padding-right: 5px"><b>To Estimate#:</b></td>
          <td nowrap><asp:Label ID="EndEstLabel" Width="100px" runat="server"></asp:label>          
          </td>
          </tr>
       <tr><td align="right" style="padding-right: 5px"><b>New Customer#:</b></td><td>
          <asp:TextBox ID="BegCustTextBox"  onkeyup="samevalue()"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>            
      </tr>   
      <tr><td align="right" style="padding-right: 5px"><b>New Customer Part#:</b></td><td>
          <asp:TextBox ID="custpartTextBox"  onkeyup="samevalue()"  width="100px" runat="server"></asp:TextBox>          
          </td>            
      </tr> 
       </table>
       </fieldset>
       </td></tr>
                        
                           
          
          </table> 
         </ContentTemplate>
                        </asp:UpdatePanel>
        <table>
        <tr><td colspan="3">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Start Process" />          
          </td></tr>
        </table>                        
       </fieldset>
          
          
          
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


