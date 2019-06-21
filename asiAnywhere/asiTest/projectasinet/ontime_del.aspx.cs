
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

public partial class on_time_del_report : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        //Response.Write(Session["mypath"]);

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["mypath"] != null)
        {
            OutputLabel.Visible = true;
            HyperLink1.Visible = true;
            HyperLink1.Text = Convert.ToString(Session["mypath"]);
            HyperLink1.NavigateUrl = @"/pdfs/" + Convert.ToString(Session["mypath"]);
        }
        else
        {
            OutputLabel.Visible = false;
            HyperLink1.Visible = false;
        }
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "ontime_del.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
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
                        TextBox1.Text = aDefaultCust;
                        TextBox2.Text = aDefaultCust;
                    }
                    catch { }
                    
                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'ontime_del.aspx'";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {                            
                            TextBox3.Text = dr["field3"].ToString();
                            TextBox4.Text = dr["field4"].ToString();
                            TextBox5.Text = dr["field5"].ToString();
                            TextBox6.Text = dr["field6"].ToString();
                            TextBox7.Text = dr["field7"].ToString();
                            TextBox8.Text = dr["field8"].ToString();

                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox3.Checked = true;
                            else
                                CheckBox3.Checked = false;
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
                        //Label begin = (Label)FormView2.FindControl("CustLabel");
                        //TextBox1.Text = begin.Text;
                        //TextBox2.Text = begin.Text;
                        TextBox2.ReadOnly = true;
                        Image4.Visible = false;
                        if (TextBox4.Text == "")
                        {
                            TextBox4.Text = "zzzzzzzzzzzzzzz";
                        }
                        if (TextBox5.Text == "")
                            TextBox5.Text = "09/20/2008";
                        if (TextBox6.Text == "")
                            TextBox6.Text = "12/31/9999";
                        if (TextBox7.Text == "")
                            TextBox7.Text = "09/20/2008";
                        if (TextBox8.Text == "")
                            TextBox8.Text = "12/31/9999";
                        
                    }
                    catch { }

                    if (Session["User"] != null)
                    {

                        lblUser.Text = UserLogin.UserName;

                    }
                }
            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'ontime_del.aspx'";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            TextBox1.Text = dr["field1"].ToString();
                            TextBox2.Text = dr["field2"].ToString();
                            TextBox3.Text = dr["field3"].ToString();
                            TextBox4.Text = dr["field4"].ToString();
                            TextBox5.Text = dr["field5"].ToString();
                            TextBox6.Text = dr["field6"].ToString();
                            TextBox7.Text = dr["field7"].ToString();
                            TextBox8.Text = dr["field8"].ToString();

                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox3.Checked = true;
                            else
                                CheckBox3.Checked = false;

                            
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

                    if (TextBox4.Text == "")
                    {
                        TextBox4.Text = "zzzzzzzzzzzzzzz";
                    }
                    if (TextBox5.Text == "")
                        TextBox5.Text = "09/20/2008";
                    if (TextBox6.Text == "")
                        TextBox6.Text = "12/31/9999";
                    if (TextBox7.Text == "")
                        TextBox7.Text = "09/20/2008";
                    if (TextBox8.Text == "")
                        TextBox8.Text = "12/31/9999";


                    if (Session["User"] != null)
                    {
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

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {

        if (CheckBox1.Checked)
        {
            HiddenField1.Value = "Yes";
        }

        if (CheckBox2.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        if (CheckBox3.Checked)
        {
            HiddenField3.Value = "Yes";
        }

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "ontime_del.aspx";
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
                ObjectDataSource1.SelectParameters["prmActTime"].DefaultValue = "OntimeRel";
                ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginItem"].DefaultValue = TextBox3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegindate"].DefaultValue = TextBox5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEnddate"].DefaultValue = TextBox6.Text.Trim();

                ObjectDataSource1.SelectParameters["prmBeginBol"].DefaultValue = TextBox7.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndBol"].DefaultValue = TextBox8.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPrintwei"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmPrintmsf"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmPrinttra"].DefaultValue = HiddenField3.Value;
            }
            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmActTime"].DefaultValue = "OntimeRel";
                ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBeginItem"].DefaultValue = TextBox3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegindate"].DefaultValue = TextBox5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEnddate"].DefaultValue = TextBox6.Text.Trim();

                ObjectDataSource1.SelectParameters["prmBeginBol"].DefaultValue = TextBox7.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndBol"].DefaultValue = TextBox8.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPrintwei"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmPrintmsf"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmPrinttra"].DefaultValue = HiddenField3.Value;
            }
        }


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'ontime_del.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
           
            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8, chk_field1, chk_field2,chk_field3) values ('" + UserLogin.UserName + "', 'ontime_del.aspx','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + TextBox5.Text.Trim() + "','" + TextBox6.Text.Trim() + "','" + TextBox7.Text.Trim() + "','" + TextBox8.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox3.Text.Trim() + "', field4 = '" + TextBox4.Text.Trim() + "', field5 = '" + TextBox5.Text.Trim() + "', field6 = '" + TextBox6.Text.Trim() + "', field7 = '" + TextBox7.Text.Trim() + "', field8 = '" + TextBox8.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "'  where user_name = '" + UserLogin.UserName + "' and prog_name = 'ontime_del.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
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


        try
        {
            //OutputLabel.Visible = true;
            //HyperLink1.Visible = true;
            Label path = (Label)FormView1.FindControl("vFileLabel");
            if (path.Text != "")
            {
                Session["mypath"] = path.Text;
            }
            else
            {
                Session["mypath"] = null;
            }
            //if (Session["mypath"] != null)
            //{
            //    HyperLink1.Text = Convert.ToString(Session["mypath"]);
            //    HyperLink1.NavigateUrl = @"/pdfs/" + Convert.ToString(Session["mypath"]);
            //}
            Response.Write("<script>window.location.href='ontime_del.aspx'</script>");
            //if (path.Text == "")
            //{
            //    Label1.Text = "No Csv Exists";
            //    Response.Write("<script>window.location.href='ontime_del.aspx'</script>");
            //}
        }
        catch
        {
            Session["mypath"] = null;
            Response.Write("<script>alert('No Csv Exists for " + TextBox1.Text + "');</script>");
            Response.Write("<script>window.location.href='ontime_del.aspx'</script>");

        }


    }

    protected void Page_PreRender(object sender, EventArgs e)
    {
        if (FormView1.DataItemCount == 0)
        {
            Session["mypath"] = null;
        }

    }
}
