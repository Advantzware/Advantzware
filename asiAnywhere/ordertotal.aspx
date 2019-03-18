<%@ Page Language="C#" MasterPageFile="~/MasterPage.master" Title="Order Total" Debug="true" %>
<%@ Import Namespace="System.Data.SqlClient"%> 
<%@ Import Namespace="System.Data"%> 
<%@ Import Namespace="System.Linq"%> 
<script runat="server">

    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["view_order_entry_pages_with_estimate"] == null)
        {
            /*ImageButton img4 = (ImageButton)Master.FindControl("ImageButton4");
            ImageButton img5 = (ImageButton)Master.FindControl("ImageButton5");
            ImageButton img6 = (ImageButton)Master.FindControl("ImageButton6");
            ImageButton img7 = (ImageButton)Master.FindControl("ImageButton7");
            img4.Visible = false;
            img5.Visible = false;
            img6.Visible = false;
            img7.Visible = false;

            Image ack = (Image)Master.FindControl("Image2");
            ImageButton add = (ImageButton)Master.FindControl("img_btn_add");
            ack.Visible = false;
            add.Visible = false;*/
            HtmlGenericControl img4 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton4");
            HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton5");
            HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton6");
            HtmlGenericControl img7 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton7");
            img4.Attributes.Add("style", "display:none");
            img5.Attributes.Add("style", "display:none");
            img6.Attributes.Add("style", "display:none");
            img7.Attributes.Add("style", "display:none");
        }

        if (Session["view_order_entry_pages_with_estimate"] != null)
        {
           /* ImageButton img1 = (ImageButton)Master.FindControl("brwsorder");
            ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
            ImageButton img3 = (ImageButton)Master.FindControl("viewitem");

            img1.Visible = false;
            img2.Visible = false;
            img3.Visible = false;

            ImageButton img4 = (ImageButton)Master.FindControl("ImageButton1");
            ImageButton img5 = (ImageButton)Master.FindControl("ImageButton2");
            ImageButton img6 = (ImageButton)Master.FindControl("ImageButton3");

            ImageButton img11 = (ImageButton)Master.FindControl("listitem");
            img4.Visible = false;
            img5.Visible = false;
            img6.Visible = false;

            img11.Visible = false;*/
            HtmlGenericControl img1 = (HtmlGenericControl)this.Page.Master.FindControl("librowseorder");
            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("livieword");
            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liviewitem");
            img1.Attributes.Add("style", "display:none");
            img2.Attributes.Add("style", "display:none");
            img3.Attributes.Add("style", "display:none");
            HtmlGenericControl img4 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton1");
            HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton2");
            HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton3");
            HtmlGenericControl img7 = (HtmlGenericControl)this.Page.Master.FindControl("lilistitem");
            img4.Attributes.Add("style", "display:none");
            img5.Attributes.Add("style", "display:none");
            img6.Attributes.Add("style", "display:none");
            img7.Attributes.Add("style", "display:none");
        }
        
        if (Session["view_order_entry_pages"] == null)
        {
            /*ImageButton img1 = (ImageButton)Master.FindControl("ImageButton1");
            ImageButton img2 = (ImageButton)Master.FindControl("ImageButton2");
            ImageButton img3 = (ImageButton)Master.FindControl("ImageButton3");

            ImageButton img11 = (ImageButton)Master.FindControl("listitem");
            img1.Visible = false;
            img2.Visible = false;
            img3.Visible = false;

            img11.Visible = false;*/
            HtmlGenericControl img1 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton1");
            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton2");
            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton3");
            HtmlGenericControl img11 = (HtmlGenericControl)this.Page.Master.FindControl("lilistitem");
            img1.Attributes.Add("style", "display:none");
            img2.Attributes.Add("style", "display:none");
            img3.Attributes.Add("style", "display:none");
            img11.Attributes.Add("style", "display:none");
        }

        if (Session["view_order_entry_pages"] != null)
        {
            /*ImageButton img1 = (ImageButton)Master.FindControl("brwsorder");
            ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
            ImageButton img3 = (ImageButton)Master.FindControl("viewitem");

            img1.Visible = false;
            img2.Visible = false;
            img3.Visible = false;*/
            HtmlGenericControl img1 = (HtmlGenericControl)this.Page.Master.FindControl("librowseorder");
            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("livieword");
            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liviewitem");
            img1.Attributes.Add("style", "display:none");
            img2.Attributes.Add("style", "display:none");
            img3.Attributes.Add("style", "display:none");

        }
        
        if (Session["User"] != null)
        {
            
            UserClass UserLogin = (UserClass)Session["User"];
            UserClass.CheckLogin(Page);
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            FormView1.ChangeMode(FormViewMode.ReadOnly);
            
            string vUserId = UserLogin.UserName;
            string vPage = "ordertotal.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //lblComp.Text = PrmComp;
            if (vCanRun == false)
            {
                Response.Redirect("login.aspx");

            }

            

                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                try
                {
                    conn.Open();

                    string cmd = "select * from button_maintain where parent = 'order_estimate.aspx' ";
                    SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                    DataSet ds = new DataSet();
                    da.Fill(ds);

                    if (ds.Tables[0].Rows.Count == 0)
                    {
                        SqlCommand cmd_insert = new SqlCommand("insert into button_maintain (parent, name, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9,btn10,chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9,chk10) values ('order_estimate.aspx','Order Entry & Order Status','View Order','List Item','View Item','Misc Chgs','Job Status','Releases','Order Total','Item Status','Invoices','Ship Notes','True','True','True','True','True','True','True','True','True','True')", conn);
                        cmd_insert.ExecuteNonQuery();
                    }

                    foreach (DataRow dr in ds.Tables[0].Rows)
                    {
                        if (Session["view_order_entry_pages_with_estimate"] == null)
                        {
                            //ImageButton img2 = (ImageButton)Master.FindControl("vieworder");
                            //ImageButton img3 = (ImageButton)Master.FindControl("viewitem");
                            HtmlGenericControl img2 = (HtmlGenericControl)this.Page.Master.FindControl("livieword");
                            HtmlGenericControl img3 = (HtmlGenericControl)this.Page.Master.FindControl("liviewitem");
                            string[] ss1 = dr["user1"].ToString().Split(',');
                            string[] ss3 = dr["user3"].ToString().Split(',');
                            if (ss1.Contains(UserLogin.UserName))
                            {
                                //img2.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                                if (dr["chk1"].ToString() == "False")
                                    img2.Attributes.Add("style", "display:none");
                            }
                            if (ss3.Contains(UserLogin.UserName))
                            {
                                //img3.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                                if (dr["chk3"].ToString() == "False")
                                    img3.Attributes.Add("style", "display:none");
                            }
                        }

                        if (Session["view_order_entry_pages_with_estimate"] != null)
                        {

                            //ImageButton img5 = (ImageButton)Master.FindControl("ImageButton5");
                            //ImageButton img6 = (ImageButton)Master.FindControl("ImageButton6");
                            //ImageButton img7 = (ImageButton)Master.FindControl("ImageButton7");
                            HtmlGenericControl img5 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton5");
                            HtmlGenericControl img6 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton6");
                            HtmlGenericControl img7 = (HtmlGenericControl)this.Page.Master.FindControl("liImageButton7");
                            string[] ss1 = dr["user1"].ToString().Split(',');
                            string[] ss2 = dr["user2"].ToString().Split(',');
                            string[] ss3 = dr["user3"].ToString().Split(',');
                            if (ss1.Contains(UserLogin.UserName))
                            {
                                //img5.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                                if (dr["chk1"].ToString() == "False")
                                    img5.Attributes.Add("style", "display:none");
                            }
                            if (ss2.Contains(UserLogin.UserName))
                            {
                                //img6.Visible = Convert.ToBoolean(dr["chk2"].ToString());
                                if (dr["chk2"].ToString() == "False")
                                    img6.Attributes.Add("style", "display:none");
                            }
                            if (ss3.Contains(UserLogin.UserName))
                            {
                                //img7.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                                if (dr["chk3"].ToString() == "False")
                                    img7.Attributes.Add("style", "display:none");
                            }
                        }

                    }
                    conn.Close();

                }
                catch { }
            
        }
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Order Total";
        /*ImageButton ordertotal = (ImageButton)Master.FindControl("ordertotal");
        ordertotal.ImageUrl = "~/img/ordertotal1.jpg";   */             
    }

    protected void Freight_click(Object sender, EventArgs e)
    { 
        UserClass UserLogin = (UserClass)Session["User"];
            
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "CalcFreight";            
    }
    
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.ReadOnly)
            {
                Button update = (Button)FormView1.FindControl("UpdateButton");
                Button fri = (Button)FormView1.FindControl("CalFrightButton");

                Label tax = (Label)FormView1.FindControl("Tax1Label");
                Label tot = (Label)FormView1.FindControl("TotalLabel");
                Label cos = (Label)FormView1.FindControl("costLabel");

                TaxesHiddenField.Value = tax.Text;
                TotHiddenField.Value = tot.Text;
                CosHiddenField.Value = cos.Text;
                
                if (Session["view_order_entry_pages_with_estimate"] == null)
                {
                    update.Visible = false;
                    fri.Visible = false;
                }                
            }
        }
        catch { }
     }
    protected void Update_click(Object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        UserClass.CheckLogin(Page);

        TextBox Weight = (TextBox)FormView1.FindControl("Weight1TextBox");
        TextBox Tax = (TextBox)FormView1.FindControl("TaxesTextBox");        
        TextBox Freight = (TextBox)FormView1.FindControl("FreigTextBox");
        CheckBox BillF = (CheckBox)FormView1.FindControl("BillFCheckBox");        
        TextBox Tot = (TextBox)FormView1.FindControl("TotTextBox");
        TextBox Com = (TextBox)FormView1.FindControl("ComTextBox");
        TextBox Cos = (TextBox)FormView1.FindControl("CosTextBox");

        /*if (BillF.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        */       
        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "update";
        ObjectDataSource1.SelectParameters["prmWeight"].DefaultValue = Weight.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTax"].DefaultValue = TaxesHiddenField.Value;
        ObjectDataSource1.SelectParameters["prmFreight"].DefaultValue = Freight.Text.Trim(); 
        ObjectDataSource1.SelectParameters["prmFbill"].DefaultValue = BillF.Checked.ToString();
        ObjectDataSource1.SelectParameters["prmRevenue"].DefaultValue = TotHiddenField.Value;
        ObjectDataSource1.SelectParameters["prmCost"].DefaultValue = CosHiddenField.Value;
        ObjectDataSource1.SelectParameters["prmComm"].DefaultValue = Com.Text.Trim();       
    }
</script>


<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    
<script type="text/javascript" language="javascript">
    var prev_val = "";  
    var com_val = 0.00;
    var fright_val = 0.00;
    
	function get_prev_val()
	{ 
	    if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value == "")
	        com_val = 0.00;  	       
	    else
	        com_val = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value;	        
	}
    function calcom()
    {   
        var t_comm = 0.00;
        var t_cost = 0.00;               
        var add_val = 0.00; 
        
        if(isNaN(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value) && document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value==".")
        {
            document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value = "";
            return;
        }
        
        var length = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value.length;
        var last_char =  document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value.charAt(length-1);        
        if(isNaN(last_char) && last_char != ".")
        {            
            document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value.replace(last_char,"");
        }
        
        /*if(length == 6 && document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value.charAt(6) !=".")
        {
            alert("hello");
            document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value + ".";
            var rep_char =  document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value.charAt(6);
            document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value.replace(rep_char,".");
        } 
        */
                    
        prev_val = com_val; 
        t_cost = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$CosTextBox.value;
        if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value == "")
            t_comm = 0.00;
        else        
            t_comm = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value;
        
        add_val = parseFloat(t_comm) - parseFloat(prev_val);                
        t_cost = parseFloat(t_cost) + parseFloat(add_val);      
        
        document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$CosTextBox.value = t_cost.toFixed(2);        
        document.aspnetForm.ctl00$ContentPlaceHolder1$CosHiddenField.value = t_cost.toFixed(2);        
        
        if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value == "")
            com_val = 0.00;            
        else
            com_val = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$ComTextBox.value;                            
    }
    function get_prev_fright()
	{ 
	    if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value == "")
	        fright_val = 0.00;  	       
	    else
	        fright_val = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value;	        	     	        
	}
    function calFright()
    {
        var t_freight = 0.00;
        var t_cost = 0.00;
        var t_revenue = 0.00;               
        var add_val = 0.00;    
                        
        if(isNaN(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value) && document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value==".")
        {
            document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value = "";
            return;
        }
        
        var length = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value.length;
        var last_char =  document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value.charAt(length-1);        
        if(isNaN(last_char) && last_char != ".")
        {            
            document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value.replace(last_char,"");
        }
                                                         
        prev_val = fright_val; 
        
        t_cost = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$CosTextBox.value;
        if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value == "")
            t_freight = 0.00;
        else        
            t_freight = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value;
        
        add_val = parseFloat(t_freight) - parseFloat(prev_val);                
        t_cost = parseFloat(t_cost) + parseFloat(add_val); 
        
        document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$CosTextBox.value = t_cost.toFixed(2);                
        document.aspnetForm.ctl00$ContentPlaceHolder1$CosHiddenField.value = t_cost.toFixed(2);
            
        if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$BillFCheckBox.checked)                
        {
            t_revenue = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$TotTextBox.value;            
            t_revenue = parseFloat(t_revenue) + parseFloat(add_val);
            document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$TotTextBox.value = t_revenue.toFixed(2);
            document.aspnetForm.ctl00$ContentPlaceHolder1$TotHiddenField.value = t_revenue.toFixed(2);
            
            if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FrtaxTextBox.value == "True")
            {
                var tax = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$TaxesTextBox.value;
                var v_frt_tax_rate = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FrttaxrateTextBox.value;
                
                tax = parseFloat(tax) + parseFloat(add_val) * parseFloat(v_frt_tax_rate)/100;
                document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$TaxesTextBox.value = tax.toFixed(2); 
                document.aspnetForm.ctl00$ContentPlaceHolder1$TaxesHiddenField.value = tax.toFixed(2);              
            }
        }       
            
            
        if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value == "")
            fright_val = 0.00;            
        else
            fright_val = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value;                                    
    }
    function calBFright()
    {
        var cal_val = 0;
        var t_freight = 0.00;
        var t_revenue = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$TotTextBox.value; 
        if( document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value == "")
            t_freight = 0.00;
        else    
            t_freight = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FreigTextBox.value;        
        
        if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$BillFCheckBox.checked)
            cal_val = 1;
        else
            cal_val = -1;
        
        t_revenue = parseFloat(t_revenue) + parseFloat(t_freight) * cal_val;        
        document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$TotTextBox.value = t_revenue.toFixed(2);  
        document.aspnetForm.ctl00$ContentPlaceHolder1$TotHiddenField.value = t_revenue.toFixed(2);
        
        if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FrtaxTextBox.value == "True")
        {
            var tax = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$TaxesTextBox.value;
            var v_frt_tax_rate = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$FrttaxrateTextBox.value;
            tax = parseFloat(tax) + (parseFloat(t_freight) * v_frt_tax_rate/100) * cal_val;
            document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$TaxesTextBox.value = tax.toFixed(2);
            document.aspnetForm.ctl00$ContentPlaceHolder1$TaxesHiddenField.value = tax.toFixed(2);
        }            
    }
</script>
<asp:HiddenField ID="HiddenField1" runat="server">
</asp:HiddenField>
<asp:HiddenField ID="TaxesHiddenField" runat="server" />
<asp:HiddenField ID="TotHiddenField" runat="server" />
<asp:HiddenField ID="CosHiddenField" runat="server" />

    <asp:FormView ID="FormView2" runat="server" CellPadding="4" DataKeyNames="company,ord-no" OnDataBound="FormView1_DataBound"
        DataSourceID="ObjectDataSource2" ForeColor="#333333" Width="1031px">
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#2461BF" />
        
        <ItemTemplate>
            <table>
            <tr>
            <td style="width: 37px">
            <b>Order#</b>
            </td>
            <td style="width: 89px">
            <b><asp:Label ID="ord_noLabel" runat="server" Text='<%# Eval("[ord-no]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b>
            </td>
            <td style="width: 26px">
            <b>Date:</b>
            </td>
            <td style="width: 89px">
            <b><asp:Label ID="ord_dateLabel" runat="server" Text='<%# Bind("[ord-date]","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b>
            </td>
            <td style="width: 29px">
            <b>Type: </b>
            </td>
            <td style="width: 83px">
            <b><asp:Label ID="typeLabel" runat="server" Text='<%# Bind("type") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b>
            </td>
            <td style="width: 38px">
            <b>Status:</b>
            </td>
            <td style="width: 91px">
            <b><asp:Label ID="statLabel" runat="server" Text='<%# Bind("stat") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b>
            </td>
            </tr>
            </table>
           
        </ItemTemplate>
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="Selectvieworder" TypeName="Order">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="ordertotal" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    <br />
    <br />
    <asp:FormView ID="FormView1" runat="server" CellPadding="4" DataSourceID="ObjectDataSource1"
         Width="1034px">
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle  />
        
         <ItemTemplate>
            
            <table class="shade">
            <tr>
            <td style="width: 61px" align="right"><b>Weight: </b></td>
            <td style="width: 87px"><b><asp:Label ID="WeightLabel" runat="server" Text='<%# Bind("Weight1") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 67px" align="right"><b> </b></td>
            <td style="width: 124px"><b> </b></td>
            <td style="width: 66px" align="right"><b>Tax1: </b></td>
            <td style="width: 97px"><b> <asp:Label ID="Tax1Label" runat="server" Text='<%# Bind("Taxes") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            </tr>
            <tr>
            <td style="width: 61px" align="right"><b>Freight: </b></td>
            <td style="width: 87px"><b><asp:Label ID="FreightLabel" runat="server" Text='<%# Bind("Freig") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 67px" align="right"><b>Bill Freight: </b></td>
            <td style="width: 124px"><b> <asp:CheckBox ID="BillFreCheckBox" runat="server" Checked='<%# Bind("BillF") %>'
                Enabled="false" /></b></td>
            <td style="width: 66px" align="right"><b>Order Total: </b></td>
            <td style="width: 97px"><b><asp:Label ID="TotalLabel" runat="server" Text='<%# Bind("Tot") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            </tr>
            <tr>
            <td style="width: 61px;display:none"><b>Commissions: </b></td>
            <td style="width: 204px;display:none"><b><asp:Label ID="commLabel" runat="server" Text='<%# Bind("Com") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 67px" align="right"><b> </b></td>
            <td style="width: 124px"><b> </b></td>
            <td style="width: 61px;display:none"><b>Cost: </b></td>
            <td style="width: 204px;display:none"><b><asp:Label ID="costLabel" runat="server" Text='<%# Bind("Cos") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>                                             
            
            </tr>
            <tr><td colspan="6">
            <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" CommandName="edit"
                Text="Update">
            </asp:Button>
            <asp:Button ID="CalFrightButton" runat="server" CssClass="button" OnClick="Freight_click" Text="Fright" OnClientClick="return confirm('Calculate Fright?')" > </asp:Button>
            </td></tr>
             
            </table>
           
        </ItemTemplate>
        <%--<HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />--%>
        <EditItemTemplate>
           <table class="shade">
            <tr><td><b>Weight:</b></td>
            <td><asp:TextBox ID="Weight1TextBox" runat="server" MaxLength="9" Width="70px" Text='<%# Bind("Weight1") %>'> </asp:TextBox></td>
            <td></td><td></td>
            <td><b>Tax:</b></td>                
            <td><asp:TextBox ID="TaxesTextBox" runat="server" Text='<%# Bind("Taxes") %>' Enabled="false"></asp:TextBox></td></tr>                                    
            <tr><td><b>Fright:</b></td>
            <td><asp:TextBox ID="FreigTextBox" runat="server" MaxLength="9" Width="70px" onfocus="get_prev_fright()" onkeyup="calFright()" Text='<%# Bind("Freig") %>'></asp:TextBox></td>
            <td><b>Bill Fright:</b></td>
            <td><asp:CheckBox ID="BillFCheckBox" runat="server" onclick="calBFright()"  Checked='<%# Bind("BillF") %>' /></td>
            <td><b>Order Total:</b></td>
            <td><asp:TextBox ID="TotTextBox" runat="server" Text='<%# Bind("Tot") %>' Enabled="false"> </asp:TextBox></td></tr>                                    
            <tr><td><b>Commissions:</b></td>
            <td><asp:TextBox ID="ComTextBox" runat="server" MaxLength="9" Width="70px" onfocus="get_prev_val()" onkeyup="calcom()" Text='<%# Bind("Com") %>'></asp:TextBox></td>            
            <td></td><td></td>
            <td><b>Order Cost:</b></td>
            <td> <asp:TextBox ID="CosTextBox" runat="server" Text='<%# Bind("Cos") %>' Enabled="false"></asp:TextBox></td>                                                                       
            <td style="display:none"><b><asp:TextBox ID="FrtaxTextBox" runat="server" Text='<%# Eval("v-fr-tax") %>' ></asp:TextBox> </b></td> 
            <td style="display:none"><b><asp:TextBox ID="TaxrateTextBox" runat="server" Text='<%# Eval("v-tax-rate") %>' ></asp:TextBox> </b></td> 
            <td style="display:none"><b><asp:TextBox ID="FrttaxrateTextBox" runat="server" Text='<%# Eval("v-frt-tax-rate") %>' ></asp:TextBox> </b></td> 
            </tr>
             <tr><td>
             <asp:Button ID="UpdateButton" OnClick="Update_click" runat="server" CssClass="buttonM" CausesValidation="True" Text="Save"> </asp:Button>
            <asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button></td></tr> </table> 
        </EditItemTemplate>
              
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="OrderTot" TypeName="Order">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="ordertotal" Type="String" />
            <asp:Parameter Name="prmWeight" Type="Decimal" />            
            <asp:Parameter Name="prmTax" Type="Decimal" />
            <asp:Parameter Name="prmFreight" Type="Decimal" />
            <asp:Parameter Name="prmFbill" Type="String" />
            <asp:Parameter Name="prmRevenue" Type="Decimal" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmComm" Type="Decimal" />            
        </SelectParameters>
    </asp:ObjectDataSource>
</asp:Content>

