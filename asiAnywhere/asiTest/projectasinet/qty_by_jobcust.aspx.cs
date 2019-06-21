
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

public partial class Qty_Jobcust : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "qty_by_jobcust.aspx";
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

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'qty_by_jobcust.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                           // begincust_TextBox.Text = dr["field1"].ToString();
                            //endcust_TextBox.Text = dr["field1"].ToString();
                            custpo_TextBox.Text = dr["field3"].ToString();
                            endcustpo_TextBox.Text = dr["field4"].ToString();
                            besmanTextBox.Text = dr["field5"].ToString();
                            endsmanTextBox.Text = dr["field6"].ToString();

                            if (dr["rd_field1"].ToString() == "Stocked")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "Custom")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "All")
                                RadioButtonList1.SelectedIndex = 2;

                           if (dr["chk_field1"].ToString() == "Yes")
                                checkbox1.Checked = true;
                            else
                                checkbox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                checkbox3.Checked = true;
                            else
                                checkbox3.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                CheckBox4.Checked = true;
                            else
                                CheckBox4.Checked = false;
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
                       
                        endcust_TextBox.ReadOnly = true;
                        Image1.Visible = false;
                    }
                    catch { }
                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"];
                        lblUser.Text = UserLogin.UserName;
                        if (endcustpo_TextBox.Text == "")
                            endcustpo_TextBox.Text = "zzzzzzzz";
                        if (endsmanTextBox.Text == "")
                            endsmanTextBox.Text = "zzz";
                        RadioButtonList8.SelectedIndex = 0;

                    }

                }
            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Trans_login"]);
                    //Label begin = (Label)FormView2.FindControl("CustLabel");
                    //begincust_TextBox.Text = begin.Text;
                    //endcust_TextBox.Text = begin.Text;
                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'qty_by_jobcust.aspx'  ";
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

                            if (dr["rd_field1"].ToString() == "Stocked")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "Custom")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "All")
                                RadioButtonList1.SelectedIndex = 2;

                            if (dr["chk_field1"].ToString() == "Yes")
                                checkbox1.Checked = true;
                            else
                                checkbox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                checkbox3.Checked = true;
                            else
                                checkbox3.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                CheckBox4.Checked = true;
                            else
                                CheckBox4.Checked = false;
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
                        
                    }
                    catch { }
                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"];
                        lblUser.Text = UserLogin.UserName;
                        if(endcustpo_TextBox.Text == "")
                        endcustpo_TextBox.Text = "zzzzzzzz";
                        if(endsmanTextBox.Text == "")
                        endsmanTextBox.Text = "zzz";
                    RadioButtonList8.SelectedIndex = 0;
                        
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
        if (checkbox3.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "No";
        }
        if (CheckBox4.Checked)
        {
            HiddenField4.Value = "Yes";
        }
        else
        {
            HiddenField4.Value = "No";
        }
        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField5.Value = "Stocked";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField5.Value = "Custom";
        if (RadioButtonList1.SelectedIndex == 2)
            HiddenField5.Value = "All";

        if (RadioButtonList8.SelectedIndex == 0)
            HiddenField6.Value = "NO";
        if (RadioButtonList8.SelectedIndex == 1)
            HiddenField6.Value = "Yes";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "qty_by_jobcust.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            
            if (aUsers == "external")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "jobcust";
                ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Trans_login"]);
                ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = begincust_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = begincust_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginPo"].DefaultValue = custpo_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndPo"].DefaultValue = endcustpo_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeSaleman"].DefaultValue = besmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndSalesman"].DefaultValue = endsmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmItemCode"].DefaultValue = HiddenField5.Value;
                ObjectDataSource1.SelectParameters["prmZeroQty"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmInvWarehouse"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmReceipt"].DefaultValue = HiddenField3.Value;

                ObjectDataSource1.SelectParameters["prmCustPart"].DefaultValue = HiddenField4.Value;
                ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenField6.Value;
            }
            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "jobcust";
                ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Trans_login"]);
                ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = begincust_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = endcust_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginPo"].DefaultValue = custpo_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndPo"].DefaultValue = endcustpo_TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeSaleman"].DefaultValue = besmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndSalesman"].DefaultValue = endsmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmItemCode"].DefaultValue = HiddenField5.Value;
                ObjectDataSource1.SelectParameters["prmZeroQty"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmInvWarehouse"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmReceipt"].DefaultValue = HiddenField3.Value;

                ObjectDataSource1.SelectParameters["prmCustPart"].DefaultValue = HiddenField4.Value;
                ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenField6.Value;
            }
        }

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'qty_by_jobcust.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2,field3,field4,field5,field6,rd_field1,chk_field1,chk_field2,chk_field3,chk_field4) values ('" + UserLogin.UserName + "','qty_by_jobcust.aspx','" + begincust_TextBox.Text.Trim() + "','" + endcust_TextBox.Text.Trim() + "','" + custpo_TextBox.Text.Trim() + "','" + endcustpo_TextBox.Text.Trim() + "','" + besmanTextBox.Text.Trim() + "','" + endsmanTextBox.Text.Trim() + "','" + HiddenField5.Value + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begincust_TextBox.Text.Trim() + "', field2 = '" + endcust_TextBox.Text.Trim() + "', field3 = '" + custpo_TextBox.Text.Trim() + "', field4 = '" + endcustpo_TextBox.Text.Trim() + "',field5 = '" + besmanTextBox.Text.Trim() + "',field6 = '" + endsmanTextBox.Text.Trim() + "',rd_field1 = '" + HiddenField5.Value + "',chk_field1 = '" + HiddenField1.Value + "',chk_field2 = '" + HiddenField2.Value + "',chk_field3 = '" + HiddenField3.Value + "',chk_field4 = '" + HiddenField4.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='qty_by_jobcust.aspx' ", conn);
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
                    Response.Write("<script>window.location.href='qty_by_jobcust.aspx'</script>");
                }
            }
            catch
            {

            }
            //if (Label1.Text == "")
            //{
            //    Response.Write("<script>window.location.href='inv_by_salesman.aspx'</script>");
            //}
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
                    Session["qty_by_jobcust"] = path2;
                    if (path2 != "")
                    {
                        if (!Request.Browser.Browser.Contains("Safari"))
                            Response.Write("<script>window.open('print_qty_by_jobcust.aspx'); target='_blank'</script>");
                        else
                            Response.Redirect("qty_by_jobcust.aspx");
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
                        Response.Write("<script>window.open('print_qty_by_jobcust.aspx'); target='_blank'</script>");
                    else
                        Response.Redirect("qty_by_jobcust.aspx");
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
