
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

public partial class alpha_itemlist : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {

              
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "alpha_itemlist.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["alpha_login"] = PrmComp;
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

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'alpha_itemlist.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            beginitem_TextBox.Text = dr["field1"].ToString();
                            enditem_TextBox.Text = dr["field2"].ToString();
                            cate_TextBox.Text = dr["field5"].ToString();
                            endcate_TextBox.Text = dr["field6"].ToString();
                            
                            if (dr["chk_field1"].ToString() == "Yes")
                                cust_CheckBox.Checked = true;
                            else
                                cust_CheckBox.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                sort_CheckBox.Checked = true;
                            else
                                sort_CheckBox.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;
                                                       
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



                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"];
                        lblUser.Text = UserLogin.UserName;
                        Session["appha_user_login"] = lblUser.Text;
                    }
                    endcust_TextBox.ReadOnly = true;
                    Image3.Visible = false;
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                    if (enditem_TextBox.Text == "")
                        enditem_TextBox.Text = "zzzzzzzzzzzzzzz";
                    if (endcust_TextBox.Text == "")
                        endcust_TextBox.Text = "zzzzzzzz";
                    if (endcate_TextBox.Text == "")
                        endcate_TextBox.Text = "zzzzzzzz";
                    RadioButtonList8.SelectedIndex = 0;

                }
            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["alpha_login"]);
                    //try
                    //{
                    //    Label begin = (Label)FormView2.FindControl("CustLabel");
                    //    begincust_TextBox.Text = begin.Text;
                    //    endcust_TextBox.Text = "zzzzzzzz";
                    //}
                    //catch { }

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'alpha_itemlist.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            beginitem_TextBox.Text = dr["field1"].ToString();
                            enditem_TextBox.Text = dr["field2"].ToString();
                            begincust_TextBox.Text = dr["field3"].ToString();
                            endcust_TextBox.Text = dr["field4"].ToString();
                            cate_TextBox.Text = dr["field5"].ToString();
                            endcate_TextBox.Text = dr["field6"].ToString();

                            if (dr["chk_field1"].ToString() == "Yes")
                                cust_CheckBox.Checked = true;
                            else
                                cust_CheckBox.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                sort_CheckBox.Checked = true;
                            else
                                sort_CheckBox.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

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


                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"];
                        lblUser.Text = UserLogin.UserName;
                        Session["appha_user_login"] = lblUser.Text;
                    }
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                    if(enditem_TextBox.Text == "")
                    enditem_TextBox.Text = "zzzzzzzzzzzzzzz";
                    if(endcust_TextBox.Text == "")
                    endcust_TextBox.Text = "zzzzzzzz";
                    if(endcate_TextBox.Text == "")
                    endcate_TextBox.Text = "zzzzzzzz";
                    RadioButtonList8.SelectedIndex = 0;

                }

            }

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }



        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
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
        if (cust_CheckBox.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (sort_CheckBox.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }
        if (CheckBox1.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "No";
        }
        if (RadioButtonList8.SelectedIndex == 0)
            HiddenField4.Value = "No";
        if (RadioButtonList8.SelectedIndex == 1)
            HiddenField4.Value = "Yes";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "alpha_itemlist.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["alpha_login"] = PrmComp;
            if (aUsers == "external")
            {
                ObjectDataSource1.SelectParameters["vEndCust"].DefaultValue = begincust_TextBox.Text.Trim();
            }
            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["vEndCust"].DefaultValue = endcust_TextBox.Text.Trim();
            }
        }


        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["appha_user_login"]);
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Alpha";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["vBeginCust"].DefaultValue = begincust_TextBox.Text.Trim();
        
        ObjectDataSource1.SelectParameters["vBeginCat"].DefaultValue = cate_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndCat"].DefaultValue = endcate_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vBeginItem"].DefaultValue = beginitem_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vEndItem"].DefaultValue = enditem_TextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["vCustWhse"].DefaultValue = HiddenField1.Value.Trim();
        ObjectDataSource1.SelectParameters["vSort"].DefaultValue = HiddenField2.Value.Trim();
        ObjectDataSource1.SelectParameters["vZero"].DefaultValue = HiddenField3.Value.Trim();


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'alpha_itemlist.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, chk_field1, chk_field2, chk_field3) values ('" + UserLogin.UserName + "','alpha_itemlist.aspx','" + beginitem_TextBox.Text.Trim() + "','" + enditem_TextBox.Text.Trim() + "','" + begincust_TextBox.Text.Trim() + "','" + endcust_TextBox.Text.Trim() + "','" + cate_TextBox.Text.Trim() + "','" + endcate_TextBox.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + beginitem_TextBox.Text.Trim() + "', field2 = '" + enditem_TextBox.Text.Trim() + "', field3 = '" + begincust_TextBox.Text.Trim() + "', field4 = '" + endcust_TextBox.Text.Trim() + "', field5 = '" + cate_TextBox.Text.Trim() + "', field6 = '" + endcate_TextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='alpha_itemlist.aspx' ", conn);
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
                    Response.Write("<script>window.location.href='alpha_itemlist.aspx'</script>");
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
                    Session["open_alpha_list"] = path2;
                    if (path2 != "")
                    {
                        if (!Request.Browser.Browser.Contains("Safari"))
                            Response.Write("<script>window.open('print_alpha_list.aspx'); target='_blank'</script>");
                        else
                            Response.Redirect("alpha_itemlist.aspx");
                    }
                }
                else
                {
                    Label1.Text = "No Pdf Exists";
                }
            }
            catch
            {
                Label1.Text = "No Pdf Exists";
            }
            if (Label1.Text == "")
            {
                Response.Write("<script>window.location.href='alpha_itemlist.aspx'</script>");
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
                Session["open_alpha_list"] = path2;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>window.open('print_alpha_list.aspx'); target='_blank'</script>");
                    else
                        Response.Redirect("alpha_itemlist.aspx");
                }

            }
            else
            {
                Label1.Text = "No Pdf Exists";
            }
        }
        catch
        {
            Label1.Text = "No Pdf Exists";
        }
    }
}
