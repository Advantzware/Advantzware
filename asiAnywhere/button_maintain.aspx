<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="button_maintain" Codebehind="button_maintain.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Button Security Maintenance</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/> 
    <script language="javascript" type="text/javascript">
        var user1 = "";
        function userlook(obj1) {
            user1 = obj1;
            var NewWindow = window.open("user_looksql.aspx", "UserLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function UserLookup(ReturnObj1) {
            if (user1 == 1) {
                var ddl1 = document.getElementById("FormView1_ddl_user1");
                if (ddl1.value == "")
                    ddl1.value = ReturnObj1
                else
                    ddl1.value = ddl1.value + "," + ReturnObj1
            }
            if (user1 == 2) {
                var ddl2 = document.getElementById("FormView1_ddl_user2");
                if (ddl2.value == "")
                    ddl2.value = ReturnObj1
                else
                    ddl2.value = ddl2.value + "," + ReturnObj1
            }
            if (user1 == 3) {
                var ddl3 = document.getElementById("FormView1_ddl_user3");
                if (ddl3.value == "")
                    ddl3.value = ReturnObj1
                else
                    ddl3.value = ddl3.value + "," + ReturnObj1
            }
            if (user1 == 4) {
                var ddl4 = document.getElementById("FormView1_ddl_user4");
                if (ddl4.value == "")
                    ddl4.value = ReturnObj1
                else
                    ddl4.value = ddl4.value + "," + ReturnObj1
            }
            if (user1 == 5) {
                var ddl5 = document.getElementById("FormView1_ddl_user5");
                if (ddl5.value == "")
                    ddl5.value = ReturnObj1
                else
                    ddl5.value = ddl5.value + "," + ReturnObj1
            }
            if (user1 == 6) {
                var ddl6 = document.getElementById("FormView1_ddl_user6");
                if (ddl6.value == "")
                    ddl6.value = ReturnObj1
                else
                    ddl6.value = ddl6.value + "," + ReturnObj1
            }
            if (user1 == 7) {
                var ddl7 = document.getElementById("FormView1_ddl_user7");
                if (ddl7.value == "")
                    ddl7.value = ReturnObj1
                else
                    ddl7.value = ddl7.value + "," + ReturnObj1
            }
            if (user1 == 8) {
                var ddl8 = document.getElementById("FormView1_ddl_user8");
                if (ddl8.value == "")
                    ddl8.value = ReturnObj1
                else
                    ddl8.value = ddl8.value + "," + ReturnObj1
            }
            if (user1 == 9) {
                var ddl9 = document.getElementById("FormView1_ddl_user9");
                if (ddl9.value == "")
                    ddl9.value = ReturnObj1
                else
                    ddl9.value = ddl9.value + "," + ReturnObj1
            }
            if (user1 == 10) {
                var ddl10 = document.getElementById("FormView1_ddl_user10");
                if (ddl10.value == "")
                    ddl10.value = ReturnObj1
                else
                    ddl10.value = ddl10.value + "," + ReturnObj1
            }
            if (user1 == 11) {
                var ddl11 = document.getElementById("FormView1_ddl_user11");
                if (ddl11.value == "")
                    ddl11.value = ReturnObj1
                else
                    ddl11.value = ddl11.value + "," + ReturnObj1
            }
            if (user1 == 12) {
                var ddl12 = document.getElementById("FormView1_ddl_user12");
                if (ddl12.value == "")
                    ddl12.value = ReturnObj1
                else
                    ddl12.value = ddl12.value + "," + ReturnObj1
            }
            if (user1 == 13) {
                var ddl13 = document.getElementById("FormView1_ddl_user13");
                if (ddl13.value == "")
                    ddl13.value = ReturnObj1
                else
                    ddl13.value = ddl13.value + "," + ReturnObj1
            }
            if (user1 == 14) {
                var ddl14 = document.getElementById("FormView1_ddl_user14");
                if (ddl14.value == "")
                    ddl14.value = ReturnObj1
                else
                    ddl14.value = ddl14.value + "," + ReturnObj1
            }
            if (user1 == 15) {
                var ddl15 = document.getElementById("FormView1_ddl_user15");
                if (ddl15.value == "")
                    ddl15.value = ReturnObj1
                else
                    ddl15.value = ddl15.value + "," + ReturnObj1
            }
        }  
    </script>   
</head>
<body>
    <form id="form1" runat="server">
   <hd:header id="Header1" runat="server"></hd:header>
    <div>
    
         <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Button Security Maintenance &nbsp;</b></font></TD>
          
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" ></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
        <asp:HiddenField ID="HiddenField1" runat="server" />
        <asp:HiddenField ID="HiddenField2" runat="server" />
        <asp:HiddenField ID="HiddenField3" runat="server" />
        <asp:HiddenField ID="HiddenField4" runat="server" />
        <asp:HiddenField ID="HiddenField5" runat="server" />
        <asp:HiddenField ID="HiddenField6" runat="server" />
        <asp:HiddenField ID="HiddenField7" runat="server" />
        <asp:HiddenField ID="HiddenField8" runat="server" />
        <asp:HiddenField ID="HiddenField9" runat="server" />
        <asp:HiddenField ID="HiddenField10" runat="server" />
        <asp:HiddenField ID="HiddenField11" runat="server" />
        <asp:HiddenField ID="HiddenField12" runat="server" />
        <asp:HiddenField ID="HiddenField13" runat="server" />
        <asp:HiddenField ID="HiddenField14" runat="server" />
        <asp:HiddenField ID="HiddenField15" runat="server" />
        
            
    <asp:Label ID="lbl_error" runat="server" Font-Bold="true" ForeColor="red"></asp:Label>
    
           <fieldset class="shade" style="width:47%;">
            <table>
        <tr>            
               <td>     
                    <asp:DropDownList ID="ddl_main_program" OnSelectedIndexChanged="ddl_main_program_selectedindexchanged" AutoPostBack="true" runat="server" DataSourceID="SqlDataSource_select_program" DataTextField="name" DataValueField="button_id">                    
                    </asp:DropDownList>
                    <asp:SqlDataSource ID="SqlDataSource_select_program" runat="server"
                        ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" SelectCommand="SELECT distinct [name],[button_id] FROM [button_maintain]">
                    </asp:SqlDataSource>
               </td>               
            
        </tr>
    </table>  
        </fieldset>
        
                            
           
        <asp:FormView ID="FormView1" runat="server" DataSourceID="SqlDataSource_view_main" 
                OnDataBound="form_view_databound" DataKeyNames="button_id" >
            <EditItemTemplate> 
                <fieldset>
                <table class="shade" width="320">
                <tr><td align="center"><b><asp:Label ID="nameTextBox" runat="server" Text='<%# Bind("name") %>' /></b></td></tr>
                </table>
                <table class="shade" width="320"><tr><td>
                <tr><td><b>Column Name</b></td><td>User ID</td><td><b>Hide</b></td></tr>                
                <tr><td><asp:Label ID="btn1TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn1") %>' /></td>
                <td><asp:TextBox ID="ddl_user1" runat="server" Text='<%# Bind("user1") %>'></asp:TextBox>
                <a href="#" tabindex="1" onclick="userlook(1); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /> </a></td>
                <td><asp:CheckBox ID="chk1TextBox" runat="server"  />
                <asp:Label ID="chkLabel1" runat="server" Visible="false"  Text='<%# Bind("chk1") %>' />
                </td></tr> 
                <tr><td><asp:Label ID="btn2TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn2") %>' /></td>
                <td><asp:TextBox ID="ddl_user2" runat="server" Text='<%# Bind("user2") %>'></asp:TextBox>
                <a href="#" tabindex="1" onclick="userlook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>               </td>
                <td><asp:CheckBox ID="chk2TextBox" runat="server"  />
                <asp:Label ID="chkLabel2" runat="server" Visible="false"  Text='<%# Bind("chk2") %>' /></td></tr>
                <tr><td><asp:Label ID="btn3TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn3") %>' /></td>
                <td><asp:TextBox ID="ddl_user3" runat="server" Text='<%# Bind("user3") %>'></asp:TextBox>
                 <a href="#" tabindex="1" onclick="userlook(3); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                  </td>
                <td><asp:CheckBox ID="chk3TextBox" runat="server" />
                <asp:Label ID="chkLabel3" runat="server" Visible="false" Text='<%# Bind("chk3") %>' /></td></tr>
                <tr><td><asp:Label ID="btn4TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn4") %>' /></td>
                <td><asp:TextBox ID="ddl_user4" runat="server" Text='<%# Bind("user4") %>'></asp:TextBox>
                 <a href="#" tabindex="1" onclick="userlook(4); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                  </td>
                <td><asp:CheckBox ID="chk4TextBox" runat="server"  />
                <asp:Label ID="chkLabel4" runat="server" Visible="false" Text='<%# Bind("chk4") %>' /></td></tr>
                <tr><td><asp:Label ID="btn5TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn5") %>' /></td>
                <td>   <asp:TextBox ID="ddl_user5" runat="server" Text='<%# Bind("user5") %>'></asp:TextBox>
                <a href="#" tabindex="1" onclick="userlook(5); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                  </td>
                <td><asp:CheckBox ID="chk5TextBox" runat="server"  />
                <asp:Label ID="chkLabel5" runat="server" Visible="false" Text='<%# Bind("chk5") %>' /></td></tr>
                <tr><td><asp:Label ID="btn6TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn6") %>' /></td>
                <td><asp:TextBox ID="ddl_user6" runat="server" Text='<%# Bind("user6") %>'></asp:TextBox>
                 <a href="#" tabindex="1" onclick="userlook(6); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                  </td>
                <td><asp:CheckBox ID="chk6TextBox" runat="server"  />
                <asp:Label ID="chkLabel6" runat="server" Visible="false" Text='<%# Bind("chk6") %>' /></td></tr>
                <tr><td><asp:Label ID="btn7TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn7") %>' /></td>
                <td><asp:TextBox ID="ddl_user7" runat="server" Text='<%# Bind("user7") %>'></asp:TextBox>
                 <a href="#" tabindex="1" onclick="userlook(7); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                 </td>
                <td><asp:CheckBox ID="chk7TextBox" runat="server"  />
                <asp:Label ID="chkLabel7" runat="server" Visible="false" Text='<%# Bind("chk7") %>' /></td></tr>
                <tr><td><asp:Label ID="btn8TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn8") %>' /></td>
                <td><asp:TextBox ID="ddl_user8" runat="server" Text='<%# Bind("user8") %>'></asp:TextBox>
                 <a href="#" tabindex="1" onclick="userlook(8); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                  </td>
                <td><asp:CheckBox ID="chk8TextBox" runat="server"  />
                <asp:Label ID="chkLabel8" runat="server" Visible="false" Text='<%# Bind("chk8") %>' /></td></tr>
                <tr><td><asp:Label ID="btn9TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn9") %>' /></td>
                <td><asp:TextBox ID="ddl_user9" runat="server" Text='<%# Bind("user9") %>'></asp:TextBox>
                <a href="#" tabindex="1" onclick="userlook(9); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                  </td>
                <td><asp:CheckBox ID="chk9TextBox" runat="server"  />
                <asp:Label ID="chkLabel9" runat="server" Visible="false" Text='<%# Bind("chk9") %>' /></td></tr>
                <tr><td><asp:Label ID="btn10TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn10") %>' /></td>
                <td><asp:TextBox ID="ddl_user10" runat="server" Text='<%# Bind("user10") %>'></asp:TextBox>
                <a href="#" tabindex="1" onclick="userlook(10); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                 </td>
                <td><asp:CheckBox ID="chk10TextBox" runat="server"  />
                <asp:Label ID="chkLabel10" runat="server" Visible="false" Text='<%# Bind("chk10") %>' /></td></tr>
                <tr><td><asp:Label ID="btn11TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn11") %>' /></td>
                <td><asp:TextBox ID="ddl_user11" runat="server" Text='<%# Bind("user11") %>'></asp:TextBox>
                 <a href="#" tabindex="1" onclick="userlook(11); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                  </td>
                <td><asp:CheckBox ID="chk11TextBox" runat="server"  />
                <asp:Label ID="chkLabel11" runat="server" Visible="false" Text='<%# Bind("chk11") %>' /></td></tr>
                <tr><td><asp:Label ID="btn12TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn12") %>' /></td>
                <td><asp:TextBox ID="ddl_user12" runat="server" Text='<%# Bind("user12") %>'></asp:TextBox>
                 <a href="#" tabindex="1" onclick="userlook(12); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                   </td>
                <td><asp:CheckBox ID="chk12TextBox" runat="server"  />
                <asp:Label ID="chkLabel12" runat="server" Visible="false" Text='<%# Bind("chk12") %>' /></td></tr>
                <tr><td><asp:Label ID="btn13TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn13") %>' /></td>
                <td><asp:TextBox ID="ddl_user13" runat="server" Text='<%# Bind("user13") %>'></asp:TextBox>
                 <a href="#" tabindex="1" onclick="userlook(13); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                 </td>
                <td><asp:CheckBox ID="chk13TextBox" runat="server"  />
                <asp:Label ID="chkLabel13" runat="server" Visible="false" Text='<%# Bind("chk13") %>' /></td></tr>
                <tr><td><asp:Label ID="btn14TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn14") %>' /></td>
                <td><asp:TextBox ID="ddl_user14" runat="server" Text='<%# Bind("user14") %>'></asp:TextBox>
                <a href="#" tabindex="1" onclick="userlook(14); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                </td>
                <td><asp:CheckBox ID="chk14TextBox" runat="server"  />
                <asp:Label ID="chkLabel14" runat="server" Visible="false" Text='<%# Bind("chk14") %>' /></td></tr>
                <tr><td><asp:Label ID="btn15TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn15") %>' /></td>
                <td><asp:TextBox ID="ddl_user15" runat="server" Text='<%# Bind("user15") %>'></asp:TextBox>
                 <a href="#" tabindex="1" onclick="userlook(15); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>                   </td>
                <td><asp:CheckBox ID="chk15TextBox" runat="server"  />
                <asp:Label ID="chkLabel15" runat="server" Visible="false" Text='<%# Bind("chk15") %>' /></td></tr>                             
                                
                 </td></tr></table>
                 
                <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" Text="Save" OnClick="update_button_click1" />
                &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel" CssClass="button" Text="Cancel" />
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Label ID="Label16" runat="server" ForeColor="Red"  Text="Use * to hide button for all user" />
                </fieldset>
                
            </EditItemTemplate>
            
            <ItemTemplate>
                <fieldset>
                <table class="shade" width="320">
                <tr><td align="center"><b><asp:Label ID="nameTextBox" runat="server" Text='<%# Bind("name") %>' /></b></td></tr>
                </table>
                <table class="shade" width="320"><tr><td>
                <tr><td><b>Column Name</b></td><td><b>User ID</b></td><td><b>Hide</b></td></tr>                
                <tr><td><asp:Label ID="btn1TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn1") %>' /></td>
                <td><asp:Label ID="Label1" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user1") %>' /></td>
                <td><asp:CheckBox ID="chk1TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chklabel1" runat="server" Visible="false"  Text='<%# Bind("chk1") %>' />
                </td></tr> 
                <tr><td><asp:Label ID="btn2TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn2") %>' /></td>
                <td><asp:Label ID="Label2" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user2") %>' /></td>
                <td><asp:CheckBox ID="chk2TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel2" runat="server" Visible="false"  Text='<%# Bind("chk2") %>' /></td></tr>
                <tr><td><asp:Label ID="btn3TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn3") %>' /></td>
                <td><asp:Label ID="Label3" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user3") %>' /></td>
                <td><asp:CheckBox ID="chk3TextBox" Enabled="false" runat="server" />
                <asp:Label ID="chkLabel3" runat="server" Visible="false" Text='<%# Bind("chk3") %>' /></td></tr>
                <tr><td><asp:Label ID="btn4TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn4") %>' /></td>
                <td><asp:Label ID="Label4" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user4") %>' /></td>
                <td><asp:CheckBox ID="chk4TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel4" runat="server" Visible="false" Text='<%# Bind("chk4") %>' /></td></tr>
                <tr><td><asp:Label ID="btn5TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn5") %>' /></td>
                <td><asp:Label ID="Label5" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user5") %>' /></td>
                <td><asp:CheckBox ID="chk5TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel5" runat="server" Visible="false" Text='<%# Bind("chk5") %>' /></td></tr>
                <tr><td><asp:Label ID="btn6TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn6") %>' /></td>
                <td><asp:Label ID="Label6" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user6") %>' /></td>
                <td><asp:CheckBox ID="chk6TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel6" runat="server" Visible="false" Text='<%# Bind("chk6") %>' /></td></tr>
                <tr><td><asp:Label ID="btn7TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn7") %>' /></td>
                <td><asp:Label ID="Label7" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user7") %>' /></td>
                <td><asp:CheckBox ID="chk7TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel7" runat="server" Visible="false" Text='<%# Bind("chk7") %>' /></td></tr>
                <tr><td><asp:Label ID="btn8TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn8") %>' /></td>
                <td><asp:Label ID="Label8" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user8") %>' /></td>
                <td><asp:CheckBox ID="chk8TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel8" runat="server" Visible="false" Text='<%# Bind("chk8") %>' /></td></tr>
                <tr><td><asp:Label ID="btn9TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn9") %>' /></td>
                <td><asp:Label ID="Label9" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user9") %>' /></td>
                <td><asp:CheckBox ID="chk9TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel9" runat="server" Visible="false" Text='<%# Bind("chk9") %>' /></td></tr>
                <tr><td><asp:Label ID="btn10TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn10") %>' /></td>
                <td><asp:Label ID="Label10" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user10") %>' /></td>
                <td><asp:CheckBox ID="chk10TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel10" runat="server" Visible="false" Text='<%# Bind("chk10") %>' /></td></tr>
                <tr><td><asp:Label ID="btn11TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn11") %>' /></td>
                <td><asp:Label ID="Label11" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user11") %>' /></td>
                <td><asp:CheckBox ID="chk11TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel11" runat="server" Visible="false" Text='<%# Bind("chk11") %>' /></td></tr>
                <tr><td><asp:Label ID="btn12TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn12") %>' /></td>
                <td><asp:Label ID="Label12" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user12") %>' /></td>
                <td><asp:CheckBox ID="chk12TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel12" runat="server" Visible="false" Text='<%# Bind("chk12") %>' /></td></tr>
                <tr><td><asp:Label ID="btn13TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn13") %>' /></td>
                <td><asp:Label ID="Label13" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user13") %>' /></td>
                <td><asp:CheckBox ID="chk13TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel13" runat="server" Visible="false" Text='<%# Bind("chk13") %>' /></td></tr>
                <tr><td><asp:Label ID="btn14TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn14") %>' /></td>
                <td><asp:Label ID="Label14" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user14") %>' /></td>
                <td><asp:CheckBox ID="chk14TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel14" runat="server" Visible="false" Text='<%# Bind("chk14") %>' /></td></tr>
                <tr><td><asp:Label ID="btn15TextBox" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("btn15") %>' /></td>
                <td><asp:Label ID="Label15" runat="server" Width="140px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("user15") %>' /></td>
                <td><asp:CheckBox ID="chk15TextBox" Enabled="false" runat="server"  />
                <asp:Label ID="chkLabel15" runat="server" Visible="false" Text='<%# Bind("chk15") %>' /></td></tr>                             
                                
                 </td></tr></table>
                
              <asp:Button ID="Button1" runat="server" CssClass="button" Text="Update" CommandName="Edit" />
              </fieldset>
      </ItemTemplate>
   </asp:FormView>
   
         
         
        <asp:Label ID="UErrorLabel" ForeColor="red" runat="server" Text=""></asp:Label>
         
         
         <asp:SqlDataSource ID="SqlDataSource_view_main" runat="server" 
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" 
                SelectCommand="SELECT * FROM [button_maintain] WHERE ([button_id] = @button_id)">
        <SelectParameters>            
            
            <asp:ControlParameter ControlID="ddl_main_program" Name="button_id" 
                PropertyName="SelectedValue" Type="Int32" />
            </SelectParameters>
        </asp:SqlDataSource>
         
    </div>
    </form>
</body>
</html>
