
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class Inv_SalesRep : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {


       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "inv_by_SalesRep.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["Trans_login"] = PrmComp;
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {
                    try
                    {
                        string UserId = UserLogin.UserName;
                        string aDefaultCust = null;
                        string aComp = null;

                        func1 user = new func1();
                        user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                        begincust_TextBox.Text = aDefaultCust;
                        endcust_TextBox.Text = aDefaultCust;
                    }
                    catch { }
                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'inv_by_SalesRep.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            begincust_TextBox.Text = dr["field1"].ToString();
                            endcust_TextBox.Text = dr["field1"].ToString();
                            custpo_TextBox.Text = dr["field3"].ToString();
                            endcustpo_TextBox.Text = dr["field4"].ToString();
                            besmanTextBox.Text = dr["field5"].ToString();
                            endsmanTextBox.Text = dr["field6"].ToString();

                            if (dr["rd_field1"].ToString() == "Customer#")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "Item")
                                RadioButtonList1.SelectedIndex = 1;
                            
                            if (dr["chk_field1"].ToString() == "Yes")
                                checkbox1.Checked = true;
                            else
                                checkbox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                           
                        }
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }



                    try
                    {
                        OutputLabel.Visible = false;
                        HyperLink1.Visible = false;
                        Label begin = (Label)FormView2.FindControl("CustLabel");
                        begincust_TextBox.Text = begin.Text;
                        endcust_TextBox.Text = begin.Text;
                        endcust_TextBox.ReadOnly = true;
                        Image1.Visible = false;
                    }
                    catch { }
                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"];
                        lblUser.Text = UserLogin.UserName;                        
                        RadioButtonList8.SelectedIndex = 0;                        
                        if (endcustpo_TextBox.Text == "")
                            endcustpo_TextBox.Text = "zzzzzzzz";
                        if (endsmanTextBox.Text == "")
                            endsmanTextBox.Text = "zzz";

                    }

                }
            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Trans_login"]);
                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'inv_by_SalesRep.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            begincust_TextBox.Text = dr["field1"].ToString();
                            endcust_TextBox.Text = dr["field2"].ToString();
                            custpo_TextBox.Text = dr["field3"].ToString();
                            endcustpo_TextBox.Text = dr["field4"].ToString();
                            besmanTextBox.Text = dr["field5"].ToString();
                            endsmanTextBox.Text = dr["field6"].ToString();

                            if (dr["rd_field1"].ToString() == "Customer#")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "Item")
                                RadioButtonList1.SelectedIndex = 1;

                            if (dr["chk_field1"].ToString() == "Yes")
                                checkbox1.Checked = true;
                            else
                                checkbox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;
                        }
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }


                    try
                    {
                        OutputLabel.Visible = false;
                        HyperLink1.Visible = false;

                        RadioButtonList8.SelectedIndex = 0;
                        if (endcust_TextBox.Text == "")
                            endcust_TextBox.Text = "zzzzzzzz";
                        if (endcustpo_TextBox.Text == "")
                            endcustpo_TextBox.Text = "zzzzzzzz";
                        if (endsmanTextBox.Text == "")
                            endsmanTextBox.Text = "zzz";
                                                
                    }
                    catch { }
                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"];
                        lblUser.Text = UserLogin.UserName;
                       
                    }

                }
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
    }
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }
    protected void SubmitButton_Click(object sender, EventArgs e)
    {

        if (checkbox1.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (CheckBox2.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }
        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField3.Value = "Customer#";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField3.Value = "Item";
        if (RadioButtonList8.SelectedIndex == 0)
            HiddenField4.Value = "NO";
        if (RadioButtonList8.SelectedIndex == 1)
            HiddenField4.Value = "Yes";

         UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "inv_by_SalesRep.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["Trans_login"] = PrmComp;
            if (aUsers == "external")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "sales";
                ObjectDataSource1.SelectParameters["vBeginCust"].DefaultValue = begincust_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vEndCust"].DefaultValue = begincust_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vBeginPo"].DefaultValue = custpo_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vEndPo"].DefaultValue = endcustpo_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vBeSaleman"].DefaultValue = besmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vEndSalesman"].DefaultValue = endsmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vQtyonHand"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["vCustWarehouse"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["vSort"].DefaultValue = HiddenField3.Value;
                ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenField4.Value;
            }
            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "sales";
                ObjectDataSource1.SelectParameters["vBeginCust"].DefaultValue = begincust_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vEndCust"].DefaultValue = endcust_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vBeginPo"].DefaultValue = custpo_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vEndPo"].DefaultValue = endcustpo_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vBeSaleman"].DefaultValue = besmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vEndSalesman"].DefaultValue = endsmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["vQtyonHand"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["vCustWarehouse"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["vSort"].DefaultValue = HiddenField3.Value;
                ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenField4.Value;
            }
        }


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'inv_by_SalesRep.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2,field3,field4,field5,field6,rd_field1,chk_field1,chk_field2) values ('" + UserLogin.UserName + "','inv_by_SalesRep.aspx','" + begincust_TextBox.Text.Trim() + "','" + endcust_TextBox.Text.Trim() + "','" + custpo_TextBox.Text.Trim() + "','" + endcustpo_TextBox.Text.Trim() + "','" + besmanTextBox.Text.Trim() + "','" + endsmanTextBox.Text.Trim() + "','" + HiddenField3.Value + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begincust_TextBox.Text.Trim() + "', field2 = '" + endcust_TextBox.Text.Trim() + "', field3 = '" + custpo_TextBox.Text.Trim() + "', field4 = '" + endcustpo_TextBox.Text.Trim() + "',field5 = '" + besmanTextBox.Text.Trim() + "',field6 = '" + endsmanTextBox.Text.Trim() + "',rd_field1 = '" + HiddenField3.Value + "',chk_field1 = '" + HiddenField1.Value + "',chk_field2 = '" + HiddenField2.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='inv_by_SalesRep.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }

        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }



        if (RadioButtonList8.SelectedIndex == 1)
        {
            try
            {
                OutputLabel.Visible = true;
                HyperLink1.Visible = true;
                Label path = (Label)FormView1.FindControl("vFileLabel");
                HyperLink1.Text = path.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

                if (path.Text == "")
                {
                    Label1.Text = "No Csv Exists";
                    Response.Write("<script>window.location.href='inv_by_SalesRep.aspx'</script>");
                }
            }
            catch
            {

            }
            
        }
        if (RadioButtonList8.SelectedIndex == 0)
        {
            try
            {
            Label vpath = (Label)FormView1.FindControl("vFileLabel");
                
                if (vpath.Text != "")
                {
                    string path = vpath.Text;
                    string path2 = @"/pdfs/" + path;
                    Session["open_inv_by_salesman_list"] = path2;
                    if (path2 != "")
                    {
                        if (!Request.Browser.Browser.Contains("Safari"))
                            Response.Write("<script>window.open('print_invby_salesman.aspx'); target='_blank'</script>");
                        else
                            Response.Redirect("inv_by_SalesRep.aspx");
                    }
                }
                else
                {
                    //Label1.Text = "No Pdf Exists";
                }
            }
            catch
            {
                //Label1.Text = "No Pdf Exists";
            }
            if (Label1.Text == "")
            {
                Response.Write("<script>window.location.href='inv_by_SalesRep.aspx'</script>");
            }
        }
    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label vpath = (Label)FormView1.FindControl("vFileLabel");

            if (vpath.Text != "")
            {
                string path = vpath.Text;
                string path2 = @"/pdfs/" + path;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>window.open('print_invby_salesman.aspx'); target='_blank'</script>");
                    else
                        Response.Redirect("inv_by_SalesRep.aspx");
                }
            }
            else
            {
                //Label1.Text = "No Pdf Exists";
            }
        }
        catch
        {
            //Label1.Text = "No Pdf Exists";
        }
    }
}
