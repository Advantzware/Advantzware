<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="ProgramMaster_main" Codebehind="ProgramMaster.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %><!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Program Master </title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <link href="ItemInquiry.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    
</head>
<body>
    <hd:Header ID="Header1" runat="server" />

    <form id="form1" runat="server">
    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD class="greyshade" align=left nowrap><font size=+0><b>Program Master&nbsp;</b></font></TD>          
          <TD class="greyshade" vAlign="middle" align="left" nowrap style="width: 469px">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;
            Company:&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            
          </TD>                    
        </TR>
        
      </TABLE>
       <TABLE id="tblMain" cellSpacing="1" cellPadding="1" align='center' width='95%' border="0">
        <TR>
          <TD style="width: 1367px">
             <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> Program <br>           
			<asp:TextBox ID="vProgramName" runat="server" Width="65px"></asp:TextBox>
			
        		</td>
          	<td nowrap> Title <br>
            <asp:TextBox ID="vTitle" runat="server" Width="65px"></asp:TextBox>
			
                	</td>    
           		    <td nowrap> View Id <br>
            <asp:TextBox ID="vViewId" runat="server" Width="65px"></asp:TextBox>
			
                	</td>  
            		<td nowrap>Add Id <br>
            <asp:TextBox ID="vAddId" runat="server" Width="65px"></asp:TextBox>
			
                	</td>         
            		<td nowrap> Update Id<br>
            <asp:TextBox ID="vUpdateId" runat="server" Width="65px"></asp:TextBox>
			
                	</td>
                	<td nowrap> Delete Id <br>
            <asp:TextBox ID="vDeleteId" runat="server" Width="65px"></asp:TextBox>
			
                	</td>
                	
                  
                  <td nowrap>Rows/Page<br>
                    	<asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
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
                             <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Invalid Input" ControlToValidate="aLineLabel" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource></td>
			<td><asp:Button ID="btnSearch" runat="server" Text="Search" OnClick="btnSearch_Click" CssClass="button" /></td>
			<td><asp:Button ID="btn_reset" runat="server" CssClass="button" Text="Reset" OnClick="btn_reset_Click" /></td>
            
                   <%--<td id="tdInfo" runat="server" class="shade" align="center" width="100">
                       <asp:Label ID="lblCount" runat="server" Width="80px">Details found:&nbsp;0</asp:Label>
                       <br />
                      <asp:label id="lblPage" runat="server">Page&nbsp;<%=

                 (GridView1.PageCount == 0) ? 0 : GridView1.PageIndex + 1
                  %>&nbsp;of&nbsp;<%=GridView1.PageCount%></asp:label> 
                   </td>--%>
                  
                 
                  
			
                  </tr>
                  </table>
                  <%--</br>                            
                
         </td>
         </tr>--%>
         
       </table>
       </td>
      </tr>    
   
    <tr>      
        <td style="width: 1367px; height: 41px;">
       
        <asp:ObjectDataSource ID="objProgramMasterList" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectProgramMasterlist" TypeName="ProgramMaster">
            <SelectParameters>
                <asp:Parameter DefaultValue="select" Name="prmAction" Type="String" />                
                <asp:Parameter Name="programName" Type="String" />
                <asp:Parameter Name="title" Type="String" />
                <asp:Parameter Name="viewId" Type="String" />
                <asp:Parameter Name="addId" Type="String" />
                <asp:Parameter Name="updateId" Type="String" />
                <asp:Parameter Name="deleteId" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    
     
        </td>
        </tr>
        <tr width="100%">
        <td style="width: 1367px">
   
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" DataKeyNames="prgmname" AllowPaging="True" AllowSorting="True" BorderStyle="Solid" EmptyDataText="No records found"
            DataSourceID="objProgramMasterList" Width="100%" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" >
            <SelectedRowStyle CssClass="GridSelected" BackColor="yellow" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />        
            <Columns>
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" >
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:BoundField DataField="prgmname" HeaderText="Program" ReadOnly="True" SortExpression="prgmname" >
                    <ItemStyle HorizontalAlign="Left" Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="prgtitle" HeaderText="Title" SortExpression="prgtitle" >
                <ItemStyle HorizontalAlign="Left" Wrap="False" />
                    
                </asp:BoundField>
                <asp:BoundField DataField="dir_group" HeaderText="Directory" SortExpression="dir_group" >
                <ItemStyle HorizontalAlign="Left" Wrap="False" />
                    
                </asp:BoundField>
                 <asp:CheckBoxField DataField="menu_item" HeaderText="Menu" SortExpression="menu_item" >
                     <ItemStyle Wrap="True" />
                 </asp:CheckBoxField>
                <asp:BoundField DataField="can_run" HeaderText="View ID" SortExpression="can_run" >
                <ItemStyle HorizontalAlign="Left" Wrap="False" />
                    
                </asp:BoundField>
                <asp:BoundField DataField="can_create" HeaderText="Add ID" SortExpression="can_create" >
                <ItemStyle HorizontalAlign="Left" Wrap="False" />
                    
                </asp:BoundField>
                <asp:BoundField DataField="can_update" HeaderText="Update ID" SortExpression="can_update" >
                <ItemStyle HorizontalAlign="Left" Wrap="False" />
                    
                </asp:BoundField>
                <asp:BoundField DataField="can_delete" HeaderText="Delete ID" SortExpression="can_delete" >
                <ItemStyle HorizontalAlign="Left" Wrap="False" />
                    
                </asp:BoundField>
                <asp:CheckBoxField DataField="track_usage" HeaderText="Track Usage" SortExpression="track_usage" />
                <asp:CheckBoxField DataField="popup" HeaderText="Popup" SortExpression="popup" />
                <asp:BoundField DataField="mfgroup" HeaderText="Parent(s)" SortExpression="mfgroup" >
                <ItemStyle HorizontalAlign="Left" Wrap="False" />
                    
                </asp:BoundField>
                
                
               
                
            
            </Columns>
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
        </asp:GridView>
    
     
        </td>
        </tr>
        <tr width="100%">
        <td style="width: 1367px; height: 39px;">
        <asp:ObjectDataSource ID="objProgramMasterDetail" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectProgramMaster" TypeName="ProgramMaster" OnSelecting="objProgramMasterDetail_Selecting" DataObjectTypeName="ProgData" DeleteMethod="DeleteProgramMaster" InsertMethod="InsertProgramMaster" UpdateMethod="UpdateProgramMaster">
            <SelectParameters>
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter DefaultValue="" Name="prmItem" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
     
        </td>
        </tr>
        <tr width="100%">
        <td style="width: 1367px">
        <asp:FormView ID="FormView1" runat="server" DataKeyNames="prgmname" DataSourceID="objProgramMasterDetail" OnItemDeleted="FormView1_ItemDeleted" OnItemInserted="FormView1_ItemInserted" OnItemUpdated="FormView1_ItemUpdated" Height="214px" Width="983px" CellPadding="4" ForeColor="#333333" >
            <EditItemTemplate>
            <table class="shade">
            <tr>
            <td style="width: 51px"><b> Program: </b></td>
            <td style="width: 150px"><b> 
                <asp:Label ID="prgmnameLabel1" runat="server" Text='<%# Eval("prgmname") %>'></asp:Label></b></td>
            <td style="width: 92px"><b> Menu Item: </b></td>
            <td style="width: 50px"><b> 
                <asp:CheckBox ID="menu_itemCheckBox" runat="server" Checked='<%# Bind("menu_item") %>' /></b></td>
            <td style="width: 57px"><b> View:</b></td>
            <td style="width: 150px"><b> 
                <asp:TextBox ID="can_runTextBox" Width="580px" runat="server" Text='<%# Bind("can_run") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td style="width: 51px;"><b> Title:</b></td>
            <td style="width: 150px;"><b> 
                <asp:TextBox ID="prgtitleTextBox" runat="server" Text='<%# Bind("prgtitle") %>'>
                </asp:TextBox></b></td>
            <td style="width: 92px;"><b> Pop-Up:&nbsp;</b></td>
            <td style="width: 50px;"><b> 
                <asp:CheckBox ID="popupCheckBox" runat="server" Checked='<%# Bind("popup") %>' /></b></td>
            <td style="width: 57px;"><b> Add: &nbsp;</b></td>
            <td style="width: 150px;"><b> 
                <asp:TextBox ID="can_createTextBox" Width="580px" runat="server" Text='<%# Bind("can_create") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td style="width: 51px;"><b> Directory: </b></td>
            <td style="width: 150px;"><b> 
                <asp:TextBox ID="dir_groupTextBox" runat="server" Text='<%# Bind("dir_group") %>'>
                </asp:TextBox></b></td>
            <td style="width: 92px;"><b> Run Persistent:</b></td>
            <td style="width: 50px; "><b> 
                <asp:CheckBox ID="run_persistentCheckBox" runat="server" Checked='<%# Bind("run_persistent") %>' /></b></td>
            <td style="width: 57px;"><b> Update: &nbsp;</b></td>
            <td style="width: 150px;"><b> 
                <asp:TextBox ID="can_updateTextBox" Width="580px" runat="server" Text='<%# Bind("can_update") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td style="width: 51px;"><b> Version:</b></td>
            <td style="width: 150px;"><b> 
                <asp:TextBox ID="prgm_verTextBox" runat="server" Text='<%# Bind("prgm_ver") %>'>
                </asp:TextBox></b></td>
            <td style="width: 92px;"><b> Track Usage: </b></td>
            <td style="width: 50px;"><b> 
                <asp:CheckBox ID="track_usageCheckBox" runat="server" Checked='<%# Bind("track_usage") %>' /></b></td>
            <td style="width: 57px;"><b> Delete: </b></td>
            <td style="width: 150px; "><b> 
                <asp:TextBox ID="can_deleteTextBox" Width="580px" runat="server" Text='<%# Bind("can_delete") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td style="width: 51px;"><b> </b></td>
            <td style="width: 150px;"><b> </b></td>
            <td style="width: 92px; "><b> </b></td>
            <td style="width: 50px;"><b> </b></td>
            <td style="width: 57px;"><b> Parent: &nbsp;</b></td>
            <td style="width: 150px; "><b> 
                <asp:TextBox ID="mfgroupTextBox" Width="580px" runat="server" Text='<%# Bind("mfgroup") %>'>
                </asp:TextBox></b></td>
                <br />
            </tr>
            
            </table>
                <table class="shade">
                <tr>
                <td><b> <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update" CssClass="buttonM" 
                    Text="Save">
                </asp:Button></b></td>
                <td></td>
                <td></td>
                <td><b> <asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel" OnClick="Mode1_Changed" CssClass="buttonM"
                    Text="Cancel">
                </asp:Button></b></td>
                </tr>
                </table>
                
                
                
            </EditItemTemplate>
            <InsertItemTemplate>
            <table style="width: 547px">
            <tr>
            <td style="width:50px;"><b>Program:</b></td>
            <td style="width:200px;"><b><asp:TextBox ID="prgmnameTextBox" runat="server" Text='<%# Bind("prgmname") %>'>
                </asp:TextBox></b></td>
            <td style="width:50px;"><b>Title:</b></td>
            <td><b><asp:TextBox ID="prgtitleTextBox" runat="server" Text='<%# Bind("prgtitle") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b>Directory:</b></td>
            <td><b><asp:TextBox ID="dir_groupTextBox" runat="server" Text='<%# Bind("dir_group") %>'>
                </asp:TextBox></b></td>
            <td><b>Version:</b></td>
            <td><b><asp:TextBox ID="prgm_verTextBox" runat="server" Text='<%# Bind("prgm_ver") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b>menu Item:</b></td>
            <td><b> <asp:CheckBox ID="menu_itemCheckBox" runat="server" Checked='<%# Bind("menu_item") %>' /></b></td>
            <td><b>Pop Up:</b></td>
            <td><b><asp:CheckBox ID="popupCheckBox" runat="server" Checked='<%# Bind("popup") %>' /></b></td>
            </tr>
            <tr>
            <td><b>Run Persistent:</b></td>
            <td><b><asp:CheckBox ID="run_persistentCheckBox" runat="server" Checked='<%# Bind("run_persistent") %>' /></b></td>
            <td><b>Track Usage:</b></td>
            <td><b><asp:CheckBox ID="track_usageCheckBox" runat="server" Checked='<%# Bind("track_usage") %>' /></b></td>
            </tr>
            <tr>
            <td><b>View:</b></td>
            <td><b><asp:TextBox ID="can_runTextBox" runat="server" Text='<%# Bind("can_run") %>'>
                </asp:TextBox></b></td>
            <td><b>Add:</b></td>
            <td><b><asp:TextBox ID="can_createTextBox" runat="server" Text='<%# Bind("can_create") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b>Update:</b></td>
            <td><b><asp:TextBox ID="can_updateTextBox" runat="server" Text='<%# Bind("can_update") %>'>
                </asp:TextBox></b></td>
            <td><b>Delete:</b></td>
            <td><b><asp:TextBox ID="can_deleteTextBox" runat="server" Text='<%# Bind("can_delete") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b>Parent:</b></td>
            <td><b><asp:TextBox ID="mfgroupTextBox" runat="server" Text='<%# Bind("mfgroup") %>'>
                </asp:TextBox></b></td>
            <td></td>
            <td></td>
            </tr>
            </table>
               
                <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert" CssClass="buttonM"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel" CssClass="buttonM"
                    Text="Cancel">
                </asp:Button>
            </InsertItemTemplate>
            <ItemTemplate>
            <table>
            <tr>
            <td style="width: 69px" align="right"><b> Program:</b></td>
            <td style="width: 185px"><b> <asp:Label ID="prgmnameLabel" runat="server" Text='<%# Eval("prgmname") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="140px"></asp:Label></b></td>
                <td align="right" style="width: 20px">
                </td>
            <td style="width: 110px" align="right"><b> Menu Item:</b></td>
            <td style="width: 183px"><b><asp:CheckBox ID="menu_itemCheckBox" runat="server" Checked='<%# Bind("menu_item") %>'
                    Enabled="false" /> </b></td>
            <td style="width: 72px" align="right">
            </td>
            <td style="width: 42px" align="right"><b>View: </b></td>
                <td style="width: 70px">
                    <asp:Label ID="can_runLabel" runat="server" Text='<%# Bind("can_run") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="580px"></asp:Label></td>
            </tr>
            <tr>
            <td style="width: 69px" align="right"><b>Title:</b></td>
            <td style="width: 185px"><b><asp:Label ID="prgtitleLabel" runat="server" Text='<%# Bind("prgtitle") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="140px"></asp:Label></b></td>
            
                <td align="right" style="width: 20px">
                </td>
            <td style="width: 110px" align="right"><b>Pop-Up: </b></td>
            <td style="width: 183px"><b><asp:CheckBox ID="popupCheckBox" runat="server" Checked='<%# Bind("popup") %>' Enabled="false" /></b></td>
            <td style="width: 72px" align="right">
            </td>
            <td style="width: 42px" align="right"><b>Add: </b></td>
                <td style="width: 70px">
                    <asp:Label ID="can_createLabel" runat="server" Text='<%# Bind("can_create") %>' style="position: relative" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="580px"></asp:Label></td>
            
            </tr>
            <tr>
            <td style="width: 69px" align="right"><b>Directory :</b></td>
            <td style="width: 185px"><b><asp:Label ID="dir_groupLabel" runat="server" Text='<%# Bind("dir_group") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="140px"></asp:Label></b></td>
            
                <td align="right" style="width: 20px">
                </td>
            <td style="width: 110px" align="right"><b>Run Persistent:</b></td>
            <td style="width: 183px"><b><asp:CheckBox ID="run_persistentCheckBox" runat="server" Checked='<%# Bind("run_persistent") %>'
                    Enabled="false" /></b></td>
            <td style="width: 72px" align="right">
            </td>
            <td style="width: 42px" align="right"><b>Update:</b></td>
                <td style="width: 70px">
                    <asp:Label ID="can_updateLabel" runat="server" Text='<%# Bind("can_update") %>' style="position: relative" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="580px"></asp:Label></td>
            
            </tr>
            <tr>
            <td style="width: 69px" align="right"><b>Version</b></td>
            <td style="width: 185px"><b><asp:Label ID="prgm_verLabel" runat="server" Text='<%# Bind("prgm_ver") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            
                <td align="right" style="width: 20px">
                </td>
            <td style="width: 110px" align="right"><b>Track Usage:</b></td>
            <td style="width: 183px"><b></b><asp:CheckBox ID="track_usageCheckBox" runat="server" Checked='<%# Bind("track_usage") %>'
                    Enabled="false" /></td>
            <td style="width: 72px" align="right">
            </td>
            <td style="width: 42px" align="right"><b> Delete: </b></td>
                <td style="width: 70px">
                    <asp:Label ID="can_deleteLabel" runat="server" Text='<%# Bind("can_delete") %>' style="position: relative" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="580px"></asp:Label></td>
                </tr>
                
            <tr>
            <td></td>
            <td></td>
            <td></td>
            <td></td>
            <td></td>
            <td></td>
            <td style="width: 69px"><b>Parent:</b></td>
            <td style="width: 185px"><b> <asp:Label ID="mfgroupLabel" runat="server" Text='<%# Bind("mfgroup") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="580px"></asp:Label><br /></b></td></tr>
           </table>
                <br />
                &nbsp;&nbsp;
               
                <asp:Button ID="EditButton" runat="server" CausesValidation="False" CommandName="Edit" OnClick="Mode_Changed" CssClass="buttonM"
                    Text="Update">
                </asp:Button>
                <asp:Button ID="DeleteButton" runat="server" CausesValidation="False" CommandName="Delete" CssClass="buttonM"
                    Text="Delete">
                </asp:Button>
                <asp:Button ID="NewButton" runat="server" CausesValidation="False" CommandName="New" OnClick="Mode2_Changed" CssClass="buttonM"
                    Text="New">
                </asp:Button>
                <br />
            </ItemTemplate>
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#EFF3FB" />
            <InsertRowStyle BackColor="#EFF3FB" />
            <RowStyle BackColor="#EFF3FB" />
            <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
            <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        </asp:FormView>
        </td>
            </tr>
        </table> 
    </form>
        <ft:Footer ID="Footer1" runat="server" />

</body>
</html>
