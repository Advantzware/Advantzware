<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="download_cust" Codebehind="download_customer_notes.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script>
var counter = 0; 
var pattern = '^GridView1';
function Check(parentChk)
{
    var elements =  document.getElementsByTagName("INPUT");   

    for(i=0; i<elements.length;i++)
    {
        if(parentChk.checked == true)
        { 
            if( IsCheckBox(elements[i]) && IsMatch(elements[i].id))
            {
            elements[i].checked = true;
            }        
        }
        else
        {
            elements[i].checked = false;
        }      
    }         

}

function IsMatch(id)
{
    var regularExpresssion = new RegExp(pattern);
    if(id.match(regularExpresssion)) return true;
    else return false;
}
function IsCheckBox(chk)
{
    if(chk.type == 'checkbox') return true;
    else return false;
}

function AddEvent(obj, evType, fn)
{
    if (obj.addEventListener)
    {
    obj.addEventListener(evType, fn, true);
    return true;
    } 

 else if (obj.attachEvent)
 {
    var r = obj.attachEvent("on"+evType, fn);
    return r;
 }
  else
   {
    return false;
   }   
}

function AttachListener()

{

    var elements =  document.getElementsByTagName("INPUT");

   

    for(i=0; i< elements.length; i++)

    {      

        if( IsCheckBox(elements[i]) &&  IsMatch(elements[i].id))

        {

            AddEvent(elements[i],'click',CheckChild);

        }

    }   

}

function CheckChild(e)

{

    var evt = e || window.event;

   

    var obj = evt.target || evt.srcElement

 

    if(obj.checked)

    {

        if(counter < GetChildCheckBoxCount())

            { counter++; }       

    }   

           

    else

    {

       if(counter > 0) { counter--; }   

    }

      

    if(counter == GetChildCheckBoxCount())

    { document.getElementById("chkAll").checked = true; }

    else if(counter < GetChildCheckBoxCount()) { document.getElementById("chkAll").checked = false; }   

 

}

function Check(parentChk)

{

    var elements =  document.getElementsByTagName("INPUT");

   

    for(i=0; i<elements.length;i++)

    {

        if(parentChk.checked == true)

        { 

            if( IsCheckBox(elements[i]) &&  IsMatch(elements[i].id))

            {

            elements[i].checked = true;

            }        

        }

        else

        {

            elements[i].checked = false;

            // reset the counter

            counter = 0;

        }      

    }

   

    if(parentChk.checked == true)

    {

        counter = GetChildCheckBoxCount();

    }  

      

}
</script>

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Download Customer Notes</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
</head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <div>
     <TABLE id="tblTop" cellSpacing="3" border="0" Width="100%">
        <TR>
          <TD align=center nowrap><font size=+0><b>Customer Notes&nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click">Back to menu</asp:linkbutton>
          </TD> 
          <TD align="right"><font size=+0><b>Users&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            &nbsp;&nbsp;&nbsp;
            <b>Company:</b>&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE> 
    </div>
    <div>
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" align='center' width='100%' border="0">
        <TR>
          <TD style="width: 1330px; height: 60px;">
             <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" />
        		<br />
        		<br />
        		<asp:Button ID="btnAll" runat="server" Text="All" OnClick="btnAll_Click" CssClass="button" Width="40px" />
                    </td>
               
            		<td width="40px"></td>
                  <td width="40px"></td>
                  <td width="40px"></td>
                  <td><b>Customer#</b>
                  <br />
                      <asp:TextBox ID="txt_custno" runat="server"></asp:TextBox>
                  </td>
                  <td nowrap><b>Rows Per Page</b><br>
                  
                      <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2">
                          <EditItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                                  Text="Update">
                              </asp:LinkButton>
                              <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </EditItemTemplate>
                          <InsertItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                                  Text="Insert">
                              </asp:LinkButton>
                              <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </InsertItemTemplate>
                          <ItemTemplate>
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                               <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
                 </td>
			               
                  </tr>
                  </table>
               </TD></TR>  
       </table></TD></TR></TABLE>
             
            
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True" 
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade"  />
        <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            <Columns>
             <asp:TemplateField>
             <HeaderTemplate>
<input type="checkbox" id="chkAll" name="chkAll" onclick="Check(this)" />

</HeaderTemplate>
    <ItemTemplate>
    <asp:CheckBox ID="chk1" runat="server" />
    </ItemTemplate>
    </asp:TemplateField>
            
                <asp:TemplateField HeaderText="Cust No" SortExpression="cust_no">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("cust_no") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("cust_no") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Contact" SortExpression="contact">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("contact") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label2" runat="server" Text='<%# Bind("contact") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Note Date" SortExpression="note_date">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox3" runat="server" Text='<%# Bind("note_date") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("note_date") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Note Time" SortExpression="note_time">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox4" runat="server" Text='<%# Bind("note_time") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label4" runat="server" Text='<%# Bind("note_time") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Viewed" SortExpression="viewed">
                    <EditItemTemplate>
                        <asp:CheckBox ID="CheckBox1" runat="server" Checked='<%# Bind("viewed") %>' />
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:CheckBox ID="CheckBox1" runat="server" Checked='<%# Bind("viewed") %>' Enabled="false" />
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Note Title" SortExpression="note_title">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox5" runat="server" Text='<%# Bind("note_title") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label5" runat="server" Text='<%# Bind("note_title") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="UserID" SortExpression="user_id">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox6" runat="server" Text='<%# Bind("user_id") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label6" runat="server" Text='<%# Bind("user_id") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Note Text" Visible="false" SortExpression="note_text">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox7" runat="server" Text='<%# Bind("note_text") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label7" runat="server" Text='<%# Bind("note_text") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="rec_key" Visible="false" SortExpression="rec_key">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox8" runat="server" Text='<%# Bind("rec_key") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label8" runat="server" Text='<%# Bind("rec_key") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                
                
                
                
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="custnotes" TypeName="download">
            <SelectParameters>
                <asp:Parameter DefaultValue="select" Name="prmAction" Type="String" />
                
                <asp:SessionParameter SessionField="dc_user_login" DefaultValue="" Name="prmUser" Type="String" />
                <asp:SessionParameter SessionField="dc_user_comp" Name="prmComp" Type="String" DefaultValue="" />
                <asp:Parameter Name="prmCust" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        <br />
        <asp:Button ID="InsertButton" runat="server" CssClass="buttonM" Text="Download" OnClick="InsertButton_Click" />
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
